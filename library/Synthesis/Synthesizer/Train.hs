{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.Train (module Synthesis.Synthesizer.Train) where

import           System.Random                 (StdGen, mkStdGen)
import           System.Timeout                (timeout)
import           System.Directory              (createDirectoryIfMissing)
import           System.CPUTime
import           Data.Foldable                 (foldrM)
import           Data.Maybe                    (fromMaybe)
import           Data.Set                      (Set, empty, insert)
import qualified Data.Set
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.HashMap.Lazy             (HashMap, (!), elems, keys, size, mapWithKey, filterWithKey)
import qualified Data.Csv as Csv
import           Data.Text.Prettyprint.Doc (pretty)
import           Text.Printf
import           Foreign.Marshal.Utils         (fromBool)
import           Control.Monad                 (join, replicateM, forM, void, when)
import           Language.Haskell.Exts.Syntax  ( Exp (..) )
import           Prelude                        hiding (abs)
import           Language.Haskell.Interpreter  ( Interpreter, liftIO, lift )
import           GHC.Exts
import           GHC.Generics                  (Generic)
import           GHC.TypeNats                  (KnownNat, Nat, type (*), type (-))
import qualified Torch.Functional.Internal     as I
import qualified Torch.DType                   as D
import qualified Torch.Tensor                  as D
import qualified Torch.Device                  as D
import qualified Torch.TensorFactories         as D
import qualified Torch.TensorOptions           as D
import qualified Torch.Optim                   as D
import qualified Torch.Serialize               as D
import qualified Torch.Autograd                as D
import qualified Torch.Functional              as F
import qualified Torch.NN                      as A
import           Torch.Typed.NN.Recurrent.LSTM
import           Torch.Typed.Aux
import           Torch.Typed.Tensor
import           Torch.Typed.NN
import           Torch.Typed.Parameter
import qualified Torch.Typed.Parameter
import           Torch.Typed.Factories
import           Torch.Typed.Optim
import           Torch.Typed.Functional
import           Torch.Typed.Autograd
import           Torch.Typed.Serialize
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical as Categorical

import           Synthesis.Orphanage ()
import           Synthesis.Data hiding (GridSearchConfig(..), EvolutionaryConfig(..))
import           Synthesis.Utility
import           Synthesis.Ast
import           Synthesis.Generation
import           Synthesis.FindHoles
import           Synthesis.Hint
import           Synthesis.Types
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.Synthesizer
import           Synthesis.Synthesizer.Params

-- TODO: can't put this in Utility right now as it'd create a circular dependency with Categorical... still need to resolve.
-- | use a Categorical distribution to sample indices from a probability tensor
sampleIdxs :: D.Tensor -> IO [Int]
sampleIdxs t = do
    let ps :: D.Tensor = flip I.unsqueeze 0 . F.flattenAll $ t
    [[idx]] :: [[Int]] <- D.asValue <$> Distribution.sample (Categorical.fromProbs ps) [1]
    return $ unravelIdx t idx

-- | deterministically pick the most likely expansion to fill a hole in a PPT
-- | deprecated, not in use
-- TODO: replace this whole block with argmaxAll (argmax_t) returning non-flat index (like np.unravel_index)...
-- (hole_idx, rule_idx) :: (Int, Int) = unravelIdx . argmaxAll $ hole_expansion_probs
argmaxExpansion :: forall num_holes rules device . Tensor device 'D.Float '[num_holes, rules] -> (Int, Int)
argmaxExpansion hole_expansion_probs = (hole_idx, rule_idx) where
    (hole_dim, rule_dim) :: (Int, Int) = (0, 1)
    rule_idx_by_hole :: Tensor device 'D.Int64 '[num_holes] =
            asUntyped (F.argmax (F.Dim rule_dim) F.RemoveDim) hole_expansion_probs
    num_holes_ :: Int = shape' rule_idx_by_hole !! 0
    best_prob_by_hole :: Tensor device 'D.Float '[num_holes] =
            UnsafeMkTensor . D.reshape [num_holes_] $ I.gather  -- F.squeeze 1
                (toDynamic hole_expansion_probs)
                rule_dim
                (toDynamic $ unsqueeze @1 rule_idx_by_hole)
                False
    hole_idx :: Int = D.asValue $ F.argmax (F.Dim 0) F.RemoveDim $ toDynamic best_prob_by_hole
    rule_idx :: Int = D.asValue $ D.select (toDynamic rule_idx_by_hole) 0 hole_idx

-- | fill a non-terminal leaf node in a PPT given hole/rule expansion probabilities
predictHole :: forall num_holes rules device . [(String, Expr)] -> Expr -> Set String -> Tensor device 'D.Float '[num_holes, rules] -> IO (Expr, Set String)
predictHole variants ppt used hole_expansion_probs = do
    [hole_idx, rule_idx] :: [Int] <- sampleIdxs . softmaxAll . toDynamic $ hole_expansion_probs
    -- order of rules: comes from `rule_emb`, which is just randomly assigned,
    -- so we can just arbitrarily associate this with any deterministic order e.g. that of `variants`
    let (_rule_str, rule_expr) :: (String, Expr) = variants !! rule_idx
    let block_name :: String = pp . head . fnAppNodes $ rule_expr
    let used' :: Set String = insert block_name used
    -- grab hole lenses by `findHolesExpr` and ensure `forwardPass` follows the same order to make these match.
    let (_hole_getter, hole_setter) :: (Expr -> Expr, Expr -> Expr -> Expr) =
            findHolesExpr ppt !! hole_idx
    let ppt' :: Expr = hole_setter ppt rule_expr
    -- putStrLn . show $ (hole_idx, rule_idx, pp ppt')
    return (ppt', used')

-- | fill a random non-terminal leaf node as per `task_fn`
superviseHole :: forall device . (KnownDevice device, RandDTypeIsValid device 'D.Int64) => HashMap String Expr -> Int -> Expr -> Expr -> IO Expr
superviseHole variantMap num_holes task_fn ppt = do
    -- randomly pick a hole -- probably a good way to uniformly cover all scenarios...?
    -- technically holes closer to the root may go thru more rounds tho, creating more opportunities to randomly get picked
    hole_idx' :: Tensor device 'D.Int64 '[] <- randint 0 num_holes
    let hole_idx :: Int = toInt hole_idx'
    let (hole_getter, hole_setter) :: (Expr -> Expr, Expr -> Expr -> Expr) =
            findHolesExpr ppt !! hole_idx
    let rule_expr :: Expr = (variantMap !) . nodeRule . hole_getter $ task_fn
    let ppt' :: Expr = hole_setter ppt rule_expr
    return ppt'

-- | supervise with task program to calculate the loss of the predicted hole/rule expansion probabilities for this PPT
fillHoleTrain :: forall num_holes rules device . (KnownDevice device, RandDTypeIsValid device 'D.Int64) => HashMap String Expr -> HashMap String Int -> Expr -> Expr -> Tensor device 'D.Float '[num_holes, rules] -> IO (Expr, Tensor device 'D.Float '[num_holes])
fillHoleTrain variantMap ruleIdxs task_fn ppt hole_expansion_probs = do
    let (_hole_dim, rule_dim) :: (Int, Int) = (0, 1)
    let [num_holes, _rules] :: [Int] = shape' hole_expansion_probs
    ppt' :: Expr <- superviseHole @device variantMap num_holes task_fn ppt
    -- iterate over holes to get their intended expansion 'probabilities', used in calculating the loss
    let gold_rule_probs :: Tensor device 'D.Float '[num_holes] = UnsafeMkTensor . D.toDevice (deviceVal @device) . D.asTensor $ getGold . fst <$> findHolesExpr ppt
            where getGold = \gtr -> (ruleIdxs !) . nodeRule . gtr $ task_fn
    return (ppt', gold_rule_probs)

-- | calculate the loss by comparing the predicted expansions to the intended programs
calcLoss :: forall rules ruleFeats device shape synthesizer num_holes . (KnownDevice device, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat ruleFeats, Synthesizer device shape rules ruleFeats synthesizer) => HashMap String Expr -> Expr -> Tp -> HashMap String Int -> synthesizer -> Tensor device 'D.Float shape -> HashMap String Expr -> HashMap String Int -> HashMap String Int -> Int -> Bool -> [(String, Expr)] -> Tensor device 'D.Float '[rules, ruleFeats] -> Interpreter (Tensor device 'D.Float '[])
calcLoss dsl task_fn taskType symbolIdxs model io_feats variantMap ruleIdxs variant_sizes max_holes maskBad variants rule_tp_emb = do
    let (_hole_dim, rule_dim) :: (Int, Int) = (0, 1)
    (_program, golds, predictions, _filled) :: (Expr, [D.Tensor], [D.Tensor], Int) <- let
            fill = \(ppt, golds, predictions, filled) -> do
                    --  :: Tensor device 'D.Float '[num_holes, rules]
                    let predicted = predict @device @shape @rules @ruleFeats @synthesizer model symbolIdxs ppt rule_tp_emb io_feats
                    predicted' <- if not maskBad then pure predicted else do
                        --  :: Tensor device 'D.Float '[num_holes, rules]
                        mask <-
                            fmap (Torch.Typed.Tensor.toDType @'D.Float . UnsafeMkTensor . D.asTensor) $
                            (\(hole_getter, hole_setter) -> mapM (fitExpr . hole_setter ppt . snd) variants)
                            `mapM` findHolesExpr ppt
                        return . Torch.Typed.Tensor.toDevice @device . asUntyped (F.mul $ toDynamic mask) $ predicted
                    (ppt', gold) <- liftIO $ fillHoleTrain variantMap ruleIdxs task_fn ppt predicted'
                    -- putStrLn $ "ppt': " <> pp ppt'
                    return (ppt', toDynamic gold : golds, toDynamic predicted' : predictions, filled + 1)
            in while (\(expr, _, _, filled) -> hasHoles expr && filled < max_holes) fill (letIn dsl (skeleton taskType), [], [], 0 :: Int)
    let gold_rule_probs :: D.Tensor = F.cat (F.Dim 0) golds
    let hole_expansion_probs :: D.Tensor = F.cat (F.Dim 0) predictions
    let loss :: Tensor device 'D.Float '[] = patchLoss @device @shape @rules @ruleFeats model variant_sizes $ UnsafeMkTensor $ crossEntropy gold_rule_probs rule_dim hole_expansion_probs
    return loss

-- | pre-calculate DSL stuff
prep_dsl :: TaskFnDataset -> Interpreter PreppedDSL
prep_dsl TaskFnDataset{..} = do
    variantTypes :: [Tp] <- (exprType . letIn dsl' . snd) `mapM` variants
    return $ PreppedDSL variants variant_sizes task_type_ins task_io_map task_outputs symbolIdxs ruleIdxs variantMap max_holes dsl' variantTypes
    where
    variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> exprBlocks
    variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
    task_type_ins :: HashMap Expr (HashMap (Tp, Tp) [Expr]) = fmap (fmap fst) <$> fnTypeIOs
    -- combine i/o lists across type instances
    task_io_map :: HashMap Expr [(Expr, Either String Expr)] = join . elems <$> fnTypeIOs
    -- then take their outputs
    task_outputs :: HashMap Expr [Either String Expr] = fmap snd <$> task_io_map
    symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
    ruleIdxs :: HashMap String Int = indexList $ fst <$> variants
    variantMap :: HashMap String Expr = fromList variants
    -- for synthesized programs, we apply the same maximum number of holes as used to generate this dataset. this allows our synthesizer enough power to construct the desired programs, while disallowing more complex programs than those of the maximum generated complexity. this choice is arbitrary; yet to think of a nicer solution.
    max_holes = maxHoles generationCfg
    -- DSL without entries equal to their key, for constructing let-in expressions.
    -- without this, blocks identical to their keys are seen as recursive, causing non-termination
    dsl' = filterWithKey (\k v -> k /= pp v) dsl

-- | train a NSPS model and return results
train :: forall device rules shape ruleFeats synthesizer . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat ruleFeats, KnownShape shape, Synthesizer device shape rules ruleFeats synthesizer) => SynthesizerConfig -> TaskFnDataset -> synthesizer -> Interpreter [EvalResult]
train synthesizerConfig taskFnDataset init_model = do
    let SynthesizerConfig{..} = synthesizerConfig
    let TaskFnDataset{..} = taskFnDataset

    -- GENERAL

    let [train_set, validation_set, test_set] :: [[Expr]] = untuple3 datasets
    let stdGen :: StdGen = mkStdGen seed
    let init_lr :: Tensor device 'D.Float '[] = UnsafeMkTensor . D.asTensor $ learningRate
    let modelFolder = resultFolder <> "/" <> ppCfg synthesizerConfig
    liftIO $ createDirectoryIfMissing True modelFolder

    prepped_dsl <- prep_dsl taskFnDataset
    let PreppedDSL{..} = prepped_dsl

    -- MODELS
    let init_optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters init_model
    let init_state = (stdGen, init_model, init_optim, False, [], init_lr, 0.0)

    (_, model, _, _, eval_results, _, _) <- foldLoop init_state numEpochs $ \ state@(gen, model_, optim_, earlyStop, eval_results, lr, prev_acc) epoch -> if earlyStop then pure state else do
        let (train_set', gen') = fisherYates gen train_set    -- shuffle
        start <- liftIO $ getCPUTime
        -- TRAIN LOOP
        let rule_tp_emb :: Tensor device 'D.Float '[rules, ruleFeats] =
                rule_encode @device @shape @rules @ruleFeats model_ variantTypes
        let foldrM_ x xs f = foldrM f x xs
        (train_losses, model', optim') :: ([D.Tensor], synthesizer, D.Adam) <- foldrM_ ([], model_, optim_) train_set' $ \ task_fn (train_losses, model, optim) -> do
            -- putStrLn $ "task_fn: \n" <> pp task_fn
            let taskType :: Tp = fnTypes ! task_fn
            -- putStrLn $ "taskType: " <> pp taskType
            let target_tp_io_pairs :: HashMap (Tp, Tp) [(Expr, Either String Expr)] =
                    fnTypeIOs ! task_fn
            -- putStrLn $ "target_tp_io_pairs: " <> pp_ target_tp_io_pairs
            --  :: Tensor device 'D.Float '[n'1, t * (2 * Dirs * h)]
            -- sampled_feats :: Tensor device 'D.Float '[r3nnBatch, t * (2 * Dirs * h)]
            io_feats :: Tensor device 'D.Float shape <- liftIO $ encode @device @shape @rules @ruleFeats model target_tp_io_pairs
            loss :: Tensor device 'D.Float '[] <- calcLoss @rules @ruleFeats dsl' task_fn taskType symbolIdxs model io_feats variantMap ruleIdxs variant_sizes max_holes maskBad variants rule_tp_emb
            -- TODO: do once for each mini-batch / fn?
            -- (newParam, optim') <- liftIO $ D.runStep model optim (toDynamic loss) $ toDynamic lr
            (newParam, optim') <- liftIO $ doStep @device @shape @rules @ruleFeats model optim loss lr
            let model' :: synthesizer = A.replaceParameters model newParam
            return (toDynamic loss : train_losses, model', optim')

        -- aggregating over task fns, which in turn had separately aggregated over any holes encountered across the different synthesis steps (so multiple times for a hole encountered across various PPTs along the way). this is fair, right?
        let loss_train :: Tensor device 'D.Float '[] = UnsafeMkTensor . F.mean . stack' 0 $ train_losses

        end <- liftIO $ getCPUTime
        let epochSeconds :: Double = (fromIntegral (end - start)) / (10^12)

        -- EVAL
        (earlyStop, eval_results') <- whenOrM (False, eval_results) (mod epoch evalFreq == 0) $ do

            (acc_valid, loss_valid) <- evaluate @device @rules @shape @ruleFeats taskFnDataset prepped_dsl bestOf maskBad model' validation_set

            liftIO $ printf
                "Epoch: %03d. Train loss: %.4f. Validation loss: %.4f. Validation accuracy: %.4f.\n"
                epoch
                (toFloat loss_train)
                (toFloat loss_valid)
                (toFloat acc_valid)

            let modelPath = modelFolder <> printf "/%04d.pt" epoch
            liftIO $ D.save (D.toDependent <$> A.flattenParameters model') modelPath

            let eval_result = EvalResult epoch epochSeconds (toFloat loss_train) (toFloat loss_valid) (toFloat acc_valid)
            let eval_results' = eval_result : eval_results
            let earlyStop :: Bool = whenOr False (length eval_results' >= 2 * checkWindow) $ let
                    losses  :: [Float] = lossValid <$> eval_results'
                    losses' :: [Float] = take (2 * checkWindow) losses
                    (current_losses, prev_losses) = splitAt checkWindow losses'
                    current :: D.Tensor = F.mean . D.asTensor $ current_losses
                    prev    :: D.Tensor = F.mean . D.asTensor $ prev_losses
                    earlyStop :: Bool = D.asValue $ F.sub current prev `I.gtScalar` convergenceThreshold
                    in earlyStop
            when earlyStop $ debug "validation loss has converged, stopping early!"

            return $ (earlyStop, eval_results')

        let acc_valid :: Float = accValid $ head eval_results'
        -- decay the learning rate if accuracy decreases
        lr' :: Tensor device 'D.Float '[] <- case (acc_valid < prev_acc) of
            True -> do
                info "accuracy decreased, decaying learning rate!"
                return . divScalar learningDecay $ lr
            False -> pure lr

        return (gen', model', optim', earlyStop, eval_results', lr', acc_valid)

    -- write results to csv
    liftIO $ createDirectoryIfMissing True resultFolder
    let resultPath = resultFolder <> "/" <> ppCfg synthesizerConfig <> ".csv"
    let eval_results' = reverse eval_results -- we want the first epoch first
    liftIO $ BS.writeFile resultPath $ BS.packChars $ BL.unpackChars $ Csv.encodeByName evalResultHeader eval_results'
    info $ "data written to " <> resultPath

    return eval_results'

evaluate :: forall device rules shape ruleFeats synthesizer num_holes
          . ( KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat ruleFeats, KnownShape shape, Synthesizer device shape rules ruleFeats synthesizer)
         => TaskFnDataset -> PreppedDSL -> Int -> Bool -> synthesizer -> [Expr] -> Interpreter (Tensor device 'D.Float '[], Tensor device 'D.Float '[])
evaluate TaskFnDataset{..} PreppedDSL{..} bestOf maskBad model dataset = do
    let rule_tp_emb :: Tensor device 'D.Float '[rules, ruleFeats] =
                rule_encode @device @shape @rules @ruleFeats model variantTypes
    eval_stats :: [(Bool, Tensor device 'D.Float '[])] <- forM dataset $ \task_fn -> do
        let taskType :: Tp = fnTypes ! task_fn
        let type_ins :: HashMap (Tp, Tp) [Expr] = task_type_ins ! task_fn
        let target_tp_io_pairs :: HashMap (Tp, Tp) [(Expr, Either String Expr)] = fnTypeIOs ! task_fn
        let target_outputs :: [Either String Expr] = task_outputs ! task_fn

        io_feats :: Tensor device 'D.Float shape <- liftIO $ encode @device @shape @rules @ruleFeats model target_tp_io_pairs
        loss :: Tensor device 'D.Float '[] <- calcLoss @rules @ruleFeats dsl' task_fn taskType symbolIdxs model io_feats variantMap ruleIdxs variant_sizes max_holes maskBad variants rule_tp_emb

        -- sample for best of 100 predictions
        -- TODO: dedupe samples before eval to save evals?
        -- TODO: consider A* / branch-and-bound / beam search instead
        sample_matches :: [Bool] <- replicateM bestOf $ do
            -- TODO: use these ExpTypeSig type annotations
            -- TODO: split io_feats and taskType based on param type instance combo 
            (program, used, _filled) :: (Expr, Set String, Int) <- let
                    --  :: (Int, Expr) -> IO (Int, Expr)
                    fill = \(ppt, used, filled) -> do
                            --  :: Tensor device 'D.Float '[num_holes, rules]
                            let predicted = predict @device @shape @rules @ruleFeats @synthesizer model symbolIdxs ppt rule_tp_emb io_feats
                            (ppt', used') <- liftIO $ predictHole variants ppt used predicted
                            return (ppt', used', filled + 1)
                    in while (\(ppt, used, filled) -> hasHoles ppt && filled < max_holes) fill (skeleton taskType, empty, 0 :: Int)
            -- say $ pp program
            if hasHoles program then pure False else do
                let defs :: HashMap String Expr = pickKeysSafe (Data.Set.toList used) dsl'
                let program' :: Expr = if null defs then program else letIn defs program
                sane :: Bool <- fitExpr program
                if not sane then pure False else do

                    -- say $ "type_ins: " <> pp_ type_ins
                    prediction_type_ios :: HashMap (Tp, Tp) [(Expr, Either String Expr)] <- let
                            compileInput :: (Tp, Tp) -> [Expr] -> Interpreter [(Expr, Either String Expr)] = \ tp_instantiation ins -> let
                                    n :: Int = length $ unTuple' $ ins !! 0
                                    -- crash_on_error=False is slower but lets me check if it compiles.
                                    -- fitExpr already does a type-check tho, so don't repeat that here.
                                    in fnIoPairs True n program' (fst tp_instantiation) $ list ins
                            in sequence $ compileInput `mapWithKey` type_ins
                    -- say $ "prediction_type_ios: " <> pp_ prediction_type_ios
                    let prediction_io_pairs :: [(Expr, Either String Expr)] =
                            join . elems $ prediction_type_ios
                    let outputs_match :: Bool = case length target_outputs == length prediction_io_pairs of
                            False -> False
                            True -> let
                                    prediction_outputs :: [Either String Expr] = snd <$> prediction_io_pairs
                                    output_matches :: [Bool] = uncurry (==) . mapBoth pp_ <$> target_outputs `zip` prediction_outputs
                                    in and output_matches
                    return outputs_match

        let best_works :: Bool = or sample_matches
        -- let score :: Tensor device 'D.Float '[] = UnsafeMkTensor . F.mean . D.asTensor $ (fromBool :: (Bool -> Float)) <$> sample_matches
        return (best_works, loss)

    let acc  :: Tensor device 'D.Float '[] = UnsafeMkTensor . F.mean . F.toDType D.Float . D.asTensor $ fst <$> eval_stats
    let loss :: Tensor device 'D.Float '[] = UnsafeMkTensor . F.mean . stack' 0 $ toDynamic           . snd <$> eval_stats
    return (acc, loss)
