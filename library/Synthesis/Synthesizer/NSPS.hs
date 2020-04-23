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
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.NSPS (module Synthesis.Synthesizer.NSPS) where

import           System.Random                 (StdGen, mkStdGen)
import           System.Timeout                (timeout)
import           Data.Foldable                 (foldrM)
import           Data.Maybe                    (fromMaybe)
import           Data.Set                      (Set, empty, insert)
import qualified Data.Set
import           Data.HashMap.Lazy             (HashMap, (!), elems, keys, size)
import           Foreign.Marshal.Utils         (fromBool)
import           Control.Monad                 (join, replicateM, forM, void, when)
import           Language.Haskell.Exts.Syntax  ( Exp (..) )
import           Prelude                        hiding (abs)
import           Language.Haskell.Interpreter  ( Interpreter, liftIO, lift )
import           GHC.Exts
import           GHC.Generics                  (Generic)
import           GHC.TypeNats                  (KnownNat, Nat, type (*))
import qualified Torch.Functional.Internal     as I
import qualified Torch.DType                   as D
import qualified Torch.Tensor                  as D
import qualified Torch.TensorFactories         as D
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
import           Synthesis.Data hiding (GenerationConfig(..))
import           Synthesis.Utility
import           Synthesis.Ast
import           Synthesis.FindHoles
import           Synthesis.Hint
import           Synthesis.Types
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.Params

data NSPSSpec (m :: Nat) (symbols :: Nat) (rules :: Nat) (t :: Nat) (encoderBatch :: Nat) (r3nnBatch :: Nat) (maxChar :: Nat) where
  NSPSSpec :: forall m symbols rules t encoderBatch r3nnBatch maxChar
     . { encoderSpec :: LstmEncoderSpec t encoderBatch maxChar, r3nnSpec :: R3NNSpec m symbols rules t r3nnBatch }
    -> NSPSSpec m symbols rules t encoderBatch r3nnBatch maxChar
 deriving (Show)

data NSPS (m :: Nat) (symbols :: Nat) (rules :: Nat) (t :: Nat) (encoderBatch :: Nat) (r3nnBatch :: Nat) (maxChar :: Nat) where
  NSPS :: forall m symbols rules t encoderBatch r3nnBatch maxChar
        . { encoder :: LstmEncoder t encoderBatch maxChar, r3nn :: R3NN m symbols rules t r3nnBatch }
       -> NSPS m symbols rules t encoderBatch r3nnBatch maxChar
 deriving (Show, Generic)

instance ( KnownNat m, KnownNat symbols, KnownNat rules, KnownNat t, KnownNat encoderBatch, KnownNat r3nnBatch, KnownNat maxChar )
  => A.Parameterized (NSPS m symbols rules t encoderBatch r3nnBatch maxChar)

instance ( KnownNat m, KnownNat symbols, KnownNat rules, KnownNat t, KnownNat encoderBatch, KnownNat r3nnBatch, KnownNat maxChar )
  => A.Randomizable (NSPSSpec m symbols rules t encoderBatch r3nnBatch maxChar) (NSPS m symbols rules t encoderBatch r3nnBatch maxChar) where
    sample NSPSSpec {..} = NSPS
            <$> A.sample encoderSpec
            <*> A.sample r3nnSpec

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
argmaxExpansion :: forall num_holes rules . Tnsr '[num_holes, rules] -> (Int, Int)
argmaxExpansion hole_expansion_probs = (hole_idx, rule_idx) where
    (hole_dim, rule_dim) :: (Int, Int) = (0, 1)
    rule_idx_by_hole :: Tensor Dev 'D.Int64 '[num_holes] =
            asUntyped (F.argmax (F.Dim rule_dim) F.RemoveDim) hole_expansion_probs
    num_holes_ :: Int = shape' rule_idx_by_hole !! 0
    best_prob_by_hole :: Tnsr '[num_holes] =
            UnsafeMkTensor . D.reshape [num_holes_] $ I.gather  -- F.squeeze 1
                (toDynamic hole_expansion_probs)
                rule_dim
                (toDynamic $ unsqueeze @1 rule_idx_by_hole)
                False
    hole_idx :: Int = D.asValue $ F.argmax (F.Dim 0) F.RemoveDim $ toDynamic best_prob_by_hole
    rule_idx :: Int = D.asValue $ D.select (toDynamic rule_idx_by_hole) 0 hole_idx

-- | fill a non-terminal leaf node in a PPT given hole/rule expansion probabilities
predictHole :: forall num_holes rules . [(String, Expr)] -> Expr -> Set String -> Tnsr '[num_holes, rules] -> IO (Expr, Set String)
predictHole variants ppt used hole_expansion_probs = do
    [hole_idx, rule_idx] :: [Int] <- sampleIdxs . softmaxAll . toDynamic $ hole_expansion_probs
    -- order rules: comes from rule_emb, which is just randomly assigned,
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
superviseHole :: HashMap String Expr -> Int -> Expr -> Expr -> IO Expr
superviseHole variantMap num_holes task_fn ppt = do
    -- randomly pick a hole -- probably a good way to uniformly cover all scenarios...?
    -- technically holes closer to the root may go thru more rounds tho, creating more opportunities to randomly get picked
    hole_idx' :: Tensor Dev 'D.Int64 '[] <- randint 0 num_holes
    let hole_idx :: Int = toInt hole_idx'
    let (hole_getter, hole_setter) :: (Expr -> Expr, Expr -> Expr -> Expr) =
            findHolesExpr ppt !! hole_idx
    let rule_expr :: Expr = (variantMap !) . nodeRule . hole_getter $ task_fn
    let ppt' :: Expr = hole_setter ppt rule_expr
    return ppt'

-- | supervise with task program to calculate the loss of the predicted hole/rule expansion probabilities for this PPT
fillHoleTrain :: forall num_holes rules . HashMap String Expr -> HashMap String Int -> Expr -> Expr -> Tnsr '[num_holes, rules] -> IO (Expr, Tnsr '[num_holes])
fillHoleTrain variantMap ruleIdxs task_fn ppt hole_expansion_probs = do
    let (_hole_dim, rule_dim) :: (Int, Int) = (0, 1)
    let [num_holes, _rules] :: [Int] = shape' hole_expansion_probs
    ppt' :: Expr <- superviseHole variantMap num_holes task_fn ppt
    -- iterate over holes to get the loss for each
    let gold_rule_probs :: Tnsr '[num_holes] = UnsafeMkTensor . D.asTensor $ getGold . fst <$> findHolesExpr ppt
            where getGold = \gtr -> (ruleIdxs !) . nodeRule . gtr $ task_fn
    return (ppt', gold_rule_probs)

-- | calculate the loss by comparing the predicted expansions to the intended programs
calcLoss :: forall m symbols rules encoderBatch r3nnBatch t maxChar . (KnownNat m, KnownNat symbols, KnownNat t, KnownNat r3nnBatch, KnownNat maxChar) => HashMap String Expr -> Expr -> Tp -> HashMap String Int -> NSPS m symbols rules t encoderBatch r3nnBatch maxChar -> Tnsr '[r3nnBatch, t * (2 * Dirs * H)] -> HashMap String Expr -> HashMap String Int -> HashMap String Int -> Int -> IO (Tnsr '[])
calcLoss dsl task_fn taskType symbolIdxs model sampled_feats variantMap ruleIdxs variant_sizes synth_max_holes = do
    let (_hole_dim, rule_dim) :: (Int, Int) = (0, 1)
    (_program, golds, predictions, _filled) :: (Expr, [D.Tensor], [D.Tensor], Int) <- let
            fill = \(ppt, golds, predictions, filled) -> do
                    predicted <- runR3nn @symbols @m (r3nn model) symbolIdxs ppt sampled_feats
                    (ppt', gold) <- fillHoleTrain variantMap ruleIdxs task_fn ppt predicted
                    -- putStrLn $ "ppt': " <> pp ppt'
                    return (ppt', toDynamic gold : golds, toDynamic predicted : predictions, filled + 1)
            in while (\(expr, _, _, filled) -> hasHoles expr && filled < synth_max_holes) fill (letIn dsl (skeleton taskType), [], [], 0 :: Int)
    let gold_rule_probs :: D.Tensor = F.cat (F.Dim 0) golds
    let hole_expansion_probs :: D.Tensor = F.cat (F.Dim 0) predictions
    let loss :: Tnsr '[] = patchLoss @m variant_sizes (r3nn model) $ UnsafeMkTensor $ crossEntropy gold_rule_probs rule_dim hole_expansion_probs
    return loss

train :: forall m encoderBatch r3nnBatch symbols rules t n_train n_validation n_test maxChar . (KnownNat m, KnownNat encoderBatch, KnownNat r3nnBatch, KnownNat symbols, KnownNat rules, KnownNat t, KnownNat n_train, KnownNat n_validation, KnownNat n_test, KnownNat maxChar) => SynthesizerConfig -> TaskFnDataset -> Interpreter ()
train SynthesizerConfig{..} TaskFnDataset{..} = do
    let rules :: Int = natValI @rules

    -- GENERAL

    -- datasets
    let set_list = untuple3 datasets
    let [train_set, validation_set, test_set] :: [[Expr]] = set_list
    let all_sets :: [Expr] = join set_list
    let [n_train, n_validation, n_test] :: [Int] = assertEq (length <$> set_list) [natValI @n_train, natValI @n_validation, natValI @n_test]
    say $ "n_train : " <> show n_train <> ", n_validation: " <> show n_validation <> ", n_test: " <> show n_test

    -- misc
    -- device <- liftIO $ getDevice
    -- let device :: D.Device = D.Device D.CPU 0
    let stdGen :: StdGen = mkStdGen seed
    -- assert our (hole-variant) blocks match their static length
    let expr_blocks :: [(String, Expr)] = assertEqBy length rules exprBlocks
    let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
    let variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
    let symbols :: Int = assertEq (natValI @LhsSymbols + size dsl) $ natValI @symbols
    let maxChar :: Int = assertEq (size charMap + 1) $ natValI @maxChar
    say $ "symbols : " <> show symbols <> ", rules: " <> show rules <> ", maxChar: " <> show maxChar
    let lr :: Tnsr '[] = UnsafeMkTensor . D.asTensor $ learningRate

    -- pre-calculate DSL stuff
    let task_type_ins :: HashMap Expr (HashMap [Tp] [Expr]) =
            fmap (fmap fst) <$> fnInTypeInstanceOutputs
    -- combine i/o lists across type instances
    let task_io_pairs :: HashMap Expr [(Expr, Either String Expr)] =
            join . elems <$> fnInTypeInstanceOutputs
    -- then take their outputs
    let task_outputs :: HashMap Expr [Either String Expr] =
            fmap snd <$> task_io_pairs
    let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
    let ruleIdxs :: HashMap String Int = indexList $ fst <$> variants
    let variantMap :: HashMap String Expr = fromList variants
    let ios :: [(Expr, Either String Expr)] =
            join . elems $ join . elems <$> fnInTypeInstanceOutputs
    say $ show $ natValI @t
    -- let longest_string :: Int = assertEq longestString $ natValI @t
    -- say $ "longest allowed i/o string length: " <> show longest_string

    -- MODELS
    let encoder_spec :: LstmEncoderSpec t encoderBatch maxChar = LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropoutRate
    let r3nn_spec :: R3NNSpec m symbols rules t r3nnBatch = initR3nn @m @symbols @rules @t @r3nnBatch variants r3nnBatch dropoutRate
    init_model :: NSPS m symbols rules t encoderBatch r3nnBatch maxChar <- liftIO $ A.sample $ NSPSSpec @m @symbols @rules encoder_spec r3nn_spec
    let init_optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters init_model
    let init_state = (stdGen, init_model, init_optim, False, [])

    void $ foldLoop init_state numEpochs $ \ state@(gen, model_, optim_, earlyStop, eval_results) epoch -> if earlyStop then pure state else do
        let (train_set', gen') = fisherYates gen train_set    -- shuffle

        -- TRAIN LOOP
        let foldrM_ x xs f = foldrM f x xs
        (train_losses, model', optim') :: ([D.Tensor], NSPS m symbols rules t encoderBatch r3nnBatch maxChar, D.Adam) <- liftIO $ foldrM_ ([], model_, optim_) train_set' $ \ task_fn (train_losses, model, optim) -> do
            -- putStrLn $ "task_fn: \n" <> pp task_fn
            let taskType :: Tp = fnTypes ! task_fn
            -- putStrLn $ "taskType: " <> pp taskType
            let target_io_pairs :: [(Expr, Either String Expr)] =
                    task_io_pairs ! task_fn
            -- putStrLn $ "target_io_pairs: " <> pp_ target_io_pairs
            --  :: Tnsr '[n'1, t * (2 * Dirs * H)]
            io_feats <- liftIO $ lstmEncoder @encoderBatch @t (encoder model) charMap target_io_pairs
            sampled_idxs :: D.Tensor <- liftIO $ F.toDType D.Int64 <$> D.randintIO' 0 (length target_io_pairs) [r3nnBatch]
            let sampled_feats :: Tnsr '[r3nnBatch, t * (2 * Dirs * H)] = UnsafeMkTensor $ D.indexSelect (toDynamic io_feats) 0 sampled_idxs
            loss :: Tnsr '[] <- liftIO $ calcLoss dsl task_fn taskType symbolIdxs model sampled_feats variantMap ruleIdxs variant_sizes synthMaxHoles
            -- TODO: do once for each mini-batch / fn?
            (newParam, optim') <- D.runStep model optim (toDynamic loss) $ toDynamic lr
            let model' = A.replaceParameters model newParam
            return (toDynamic loss : train_losses, model', optim')

        -- aggregating over task fns, which in turn had separately aggregated over any holes encountered across the different synthesis steps (so multiple times for a hole encountered across various PPTs along the way). this is fair, right?
        let loss_train :: Tnsr '[] = UnsafeMkTensor . F.mean . stack' 0 $ train_losses

        -- EVAL
        (earlyStop, eval_results') <- whenOrM (False, eval_results) (mod epoch evalFreq == 0) $ do

            test_stats :: [(Bool, Tnsr '[])] <- forM test_set $ \task_fn -> do
                let taskType :: Tp = fnTypes                        ! task_fn
                let type_ins :: HashMap [Tp] [Expr] = task_type_ins ! task_fn
                let target_io_pairs :: [(Expr, Either String Expr)] =
                        task_io_pairs                               ! task_fn
                let target_outputs :: [Either String Expr] =
                        task_outputs                                ! task_fn

                --  :: Tnsr '[n'2, t * (2 * Dirs * H)]
                io_feats <- liftIO $ lstmEncoder @encoderBatch @t (encoder model') charMap target_io_pairs
                sampled_idxs :: D.Tensor <- liftIO $ F.toDType D.Int64 <$> D.randintIO' 0 (length target_io_pairs) [r3nnBatch]
                let sampled_feats :: Tnsr '[r3nnBatch, t * (2 * Dirs * H)] = UnsafeMkTensor $ D.indexSelect (toDynamic io_feats) 0 sampled_idxs
                loss :: Tnsr '[] <- liftIO $ calcLoss dsl task_fn taskType symbolIdxs model' sampled_feats variantMap ruleIdxs variant_sizes synthMaxHoles

                -- sample for best of 100 predictions
                sample_matches :: [Bool] <- replicateM bestOf $ do
                    -- TODO: use these ExprTypeSig type annotations
                    -- TODO: split io_feats and taskType based on param type instance combo 
                    (program, used, _filled) :: (Expr, Set String, Int) <- let
                            --  :: (Int, Expr) -> IO (Int, Expr)
                            fill = \(ppt, used, filled) -> do
                                    (ppt', used') <- liftIO $ join $ predictHole variants ppt used <$> runR3nn @symbols @m (r3nn model') symbolIdxs ppt sampled_feats
                                    return (ppt', used', filled + 1)
                            in while (\(ppt, used, filled) -> hasHoles ppt && filled < synthMaxHoles) fill (skeleton taskType, empty, 0 :: Int)
                    -- say $ pp program
                    if hasHoles program then pure False else do
                        let defs :: HashMap String Expr = pickKeysSafe (Data.Set.toList used) dsl
                        let program' :: Expr = if null defs then program else letIn defs program

                        -- say $ "type_ins: " <> pp_ type_ins
                        prediction_type_ios :: HashMap [Tp] [(Expr, Either String Expr)] <- let
                                compileInput :: [Expr] -> Interpreter [(Expr, Either String Expr)] = \ins -> let
                                        n :: Int = length $ unTuple' $ ins !! 0
                                        -- crash_on_error=False is slower but lets me check if it compiles
                                        in fnIoPairs False n program' $ list ins
                                in compileInput `mapM` type_ins
                        -- say $ "prediction_type_ios: " <> pp_ prediction_type_ios
                        let prediction_io_pairs :: [(Expr, Either String Expr)] =
                                join . elems $ prediction_type_ios
                        let outputs_match :: Bool = case length target_outputs == length prediction_io_pairs of
                                False -> False
                                True -> let
                                        prediction_outputs :: [Either String Expr] = snd <$> prediction_io_pairs
                                        output_matches :: [Bool] = uncurry (==) . mapTuple pp_ <$> target_outputs `zip` prediction_outputs
                                        in and output_matches
                        -- TODO: try non-boolean score:
                        -- let outputs_match :: Float = length (filter id output_matches) / length output_matches

                        -- -- for each param type instance combo, check if the program compiles (and get outputs...)
                        -- let type_compiles :: HashMap [Tp] Boolean = not . null <$> prediction_type_ios
                        -- let num_in_type_instances :: Int = size type_compiles
                        -- let num_in_type_instances_compile :: Int = size . filter id $ type_compiles
                        -- let num_errors :: Int = num_in_type_instances - num_in_type_instances_compile
                        -- let ratio_compiles :: Float = num_in_type_instances_compile / num_in_type_instances
                        -- let ratio_errors :: Float = 1 - ratio_compiles
                        -- -- TODO: actually use compilation feedback in score
                        return outputs_match

                let best_works :: Bool = or sample_matches
                let score :: Tnsr '[] = UnsafeMkTensor . F.mean . D.asTensor $ (fromBool :: (Bool -> Float)) <$> sample_matches
                return (not best_works, loss)
                -- return loss

            let err_test  :: Tnsr '[] = UnsafeMkTensor . F.mean . F.toDType D.Float . D.asTensor $ fst <$> test_stats
            let loss_test :: Tnsr '[] = UnsafeMkTensor . F.mean . stack' 0 $ toDynamic           . snd <$> test_stats
            -- let loss_test :: Tnsr '[] = UnsafeMkTensor . F.mean . stack' 0 $ toDynamic <$> test_stats

            say
                $  "Epoch: "                <> show epoch
                <> ". Train loss: "         <> show loss_train
                <> ". Test loss: "          <> show loss_test
                -- <> ". Test error-rate: "    <> show err_test

            liftIO $ D.save (D.toDependent <$> A.flattenParameters model') modelPath

            let earlyStop :: Bool = whenOr False (length eval_results >= 2 * checkWindow) $ let
                    losses  :: [Tnsr '[]] = id <$> eval_results
                    losses' :: [Tnsr '[]] = take (2 * checkWindow) losses
                    (current_losses, prev_losses) = splitAt checkWindow losses'
                    current :: D.Tensor = F.mean . stack' 0 $ toDynamic <$> current_losses
                    prev    :: D.Tensor = F.mean . stack' 0 $ toDynamic <$>    prev_losses
                    earlyStop :: Bool = D.asValue $ F.sub prev current `I.ltScalar` convergenceThreshold
                    in earlyStop
            when earlyStop $ say "loss has converged, stopping early!"

            let eval_result = loss_test
            return $ (earlyStop, eval_result : eval_results)

        return (gen', model', optim', earlyStop, eval_results')
