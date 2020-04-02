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

module Synthesis.Synthesizer.NSPS (
    module Synthesis.Synthesizer.NSPS
    -- NSPSSpec (..),
    -- NSPS,
    -- predictHole,
    -- train,
) where

import System.Random (StdGen, mkStdGen)
-- import Data.List (null)
import Data.Foldable (foldrM)
import Data.HashMap.Lazy (HashMap, (!), elems)
import Control.Monad (join, replicateM, forM, forM_)
import Prelude hiding (abs)
import           GHC.Exts
import           GHC.Generics (Generic)
import           GHC.TypeNats (KnownNat, Nat, type (*))  -- , Mod, Div, type (+), type (-), type (<=?)
-- import           Torch.Random (Generator)
import qualified Torch.Functional.Internal     as I
import qualified Torch.DType                   as D
import qualified Torch.Tensor                  as D
-- import qualified Torch.Device                  as D
-- import qualified Torch.Random                  as D
import qualified Torch.Optim                   as D
import qualified Torch.Serialize               as D
import qualified Torch.Autograd                as D
import qualified Torch.Functional              as F
import qualified Torch.NN                      as A
import           Torch.Typed.Aux
-- import           Torch.TensorOptions
import           Torch.Typed.Tensor
-- import           Torch.Typed.Parameter
import           Torch.Typed.Factories
-- import           Torch.Typed.Optim
import           Torch.Typed.Functional
-- import           Torch.Typed.Serialize
-- import Torch.Distributions.Distribution
-- import qualified Torch.Distributions.Categorical as Categorical
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical as Categorical

-- import           Synthesis.Types
import           Synthesis.Orphanage ()
import           Synthesis.Data hiding (GenerationConfig(..))
import           Synthesis.Utility
import           Synthesis.Ast
-- import           Synthesis.Configs
-- import           Synthesis.Blocks
import           Synthesis.FindHoles
-- import           Synthesis.Generation
import           Synthesis.Hint
import           Synthesis.Types
-- import           Synthesis.TypeGen
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import qualified Synthesis.Synthesizer.Encoder as Enc
import           Synthesis.Synthesizer.R3NN

data NSPSSpec (m :: Nat) (rules :: Nat) where
  NSPSSpec :: forall m rules
     . { encoderSpec :: BaselineMLPEncoderSpec, r3nnSpec :: R3NNSpec m rules }
    -> NSPSSpec m rules
 deriving (Show, Eq)

data NSPS (m :: Nat) (rules :: Nat)  where
  NSPS :: forall m rules
        . { encoder :: BaselineMLPEncoder, r3nn :: R3NN m rules }
       -> NSPS m rules
 deriving (Show, Generic)

instance ( KnownNat m, KnownNat rules )
  => A.Parameterized (NSPS m rules) where
  flattenParameters NSPS{..} = A.flattenParameters encoder
                            <> A.flattenParameters r3nn
  replaceOwnParameters NSPS{..} = do
    encoder' <- A.replaceOwnParameters encoder
    r3nn'    <- A.replaceOwnParameters r3nn
    return $ NSPS{ r3nn = r3nn', encoder = encoder' }

instance ( KnownNat m, KnownNat rules )
  => A.Randomizable (NSPSSpec m rules) (NSPS m rules) where
    sample NSPSSpec {..} = NSPS
            <$> A.sample encoderSpec
            <*> A.sample r3nnSpec

-- | use a Categorical distribution to sample indices from a probability tensor
sampleIdxs :: D.Tensor -> IO [Int]
sampleIdxs t = do
    let ps :: D.Tensor = flip I.unsqueeze 0 . F.flattenAll $ t
    [[idx]] :: [[Int]] <- D.asValue <$> Distribution.sample (Categorical.fromProbs ps) [1]
    return $ unravelIdx t idx

-- -- | deterministically pick the most likely expansion to fill a hole in a PPT
-- -- | deprecated, not in use
-- argmaxExpansion :: Tnsr '[num_holes, rules] -> (Int, Int)
-- argmaxExpansion hole_expansion_probs = (hole idx, rule_idx) where
--     (hole_dim, rule_dim) :: (Int, Int) = (0, 1)
--     -- TODO: replace this hole block with argmaxAll (argmax_t) returning non-flat index (like np.unravel_index)...
--     -- (hole_idx, rule_idx) :: (Int, Int) = unravelIdx . argmaxAll $ hole_expansion_probs
--     rule_idx_by_hole :: Tensor Dev 'D.Int64 '[num_holes] =
--             asUntyped (F.argmax (F.Dim rule_dim) F.RemoveDim) hole_expansion_probs
--     -- putStrLn . show . shape' $ rule_idx_by_hole
--     num_holes_ :: Int = shape' rule_idx_by_hole !! 0
--     best_prob_by_hole :: Tnsr '[num_holes] =
--         --     UnsafeMkTensor $ F.squeezeAll $ I.gather
--             -- TODO: replace with `F.squeeze 1` when available so we won't need num_holes_
--             UnsafeMkTensor . D.reshape [num_holes_] $ I.gather
--         --     UnsafeMkTensor $ I.gather
--                 (toDynamic hole_expansion_probs)
--                 rule_dim
--                 -- (D.reshape [-1, 1] $ toDynamic rule_idx_by_hole)  -- [num_holes, 1]
--                 -- (I.unsqueeze (toDynamic rule_idx_by_hole) 1)
--                 (toDynamic $ unsqueeze @1 rule_idx_by_hole)
--                 -- (toDynamic rule_idx_by_hole)
--                 False
--     -- putStrLn . show . shape' $ best_prob_by_hole
--     hole_idx :: Int = D.asValue $ F.argmax (F.Dim 0) F.RemoveDim $ toDynamic best_prob_by_hole
--     -- hole_idx :: Int = categorical gen probs
--     --         where probs = toList . Just $ best_prob_by_hole
--     rule_idx :: Int = D.asValue $ D.select (toDynamic rule_idx_by_hole) 0 hole_idx

-- | fill a non-terminal leaf node in a PPT given hole/rule expansion probabilities
predictHole :: forall num_holes rules . [(String, Expr)] -> Expr -> Tnsr '[num_holes, rules] -> IO (Int, Expr)
predictHole variants ppt hole_expansion_probs = do
    -- let (hole_dim, rule_dim) :: (Int, Int) = (0, 1)
--     let (hole_idx, rule_idx) :: (Int, Int) = argmaxExpansion hole_expansion_probs

    [hole_idx, rule_idx] :: [Int] <- sampleIdxs $ toDynamic hole_expansion_probs

--     putStrLn . show $ (hole_idx, rule_idx)
    -- let estimated_probability :: Float = D.asValue $
    --         D.select (D.select (toDynamic hole_expansion_probs) hole_dim hole_idx) 0 rule_idx
--     putStrLn . show $ estimated_probability

    -- order rules: comes from rule_emb, which is just randomly assigned,
    -- so we can just arbitrarily associate this with any deterministic order e.g. that of `variants`
--     putStrLn . show $ length variants
--     putStrLn . pp_ $ variants
    let (_rule_str, rule_expr) :: (String, Expr) = variants !! rule_idx
--     putStrLn . show $ rule_str
--     putStrLn . show . pp $ rule_expr

    -- order NumHoles: node_embs. without SrcSpanInfo Expr isn't uniquely Hashable,
    -- so grab hole lenses by `findHolesExpr` and ensure `node_embs` follows the same order.
    let (_hole_getter, hole_setter) :: (Expr -> Expr, Expr -> Expr -> Expr) =
            findHolesExpr ppt !! hole_idx
    let ppt' :: Expr = hole_setter ppt rule_expr
    putStrLn . show $ (hole_idx, rule_idx, pp ppt')     -- , estimated_probability, rule_str
    let num_holes = shape' hole_expansion_probs !! 0       -- rule_idx_by_hole
--     putStrLn . show $ num_holes
    return (num_holes-1, ppt')

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

-- | calculate the loss for a PPT given hole/rule expansion probabilities then fill a random non-terminal leaf node as per `task_fn`
fillHoleTrain :: forall num_holes rules . HashMap String Expr -> HashMap String Int -> Expr -> Expr -> Tnsr '[num_holes, rules] -> IO (Int, Expr, D.Tensor)
fillHoleTrain variantMap ruleIdxs task_fn ppt hole_expansion_probs = do
    let [num_holes, rules] :: [Int] = shape' hole_expansion_probs
    -- let rules :: Int = natValI @rules
    ppt' :: Expr <- superviseHole variantMap num_holes task_fn ppt
    -- putStrLn . show $ (hole_idx, pp ppt')

    -- supervise with task program to calculate the loss of the predicted hole/rule expansion probabilities for this PPT
    let zeroes :: [Float] = replicate rules 0.0
    -- iterate over holes to get the loss for each
    let gold_rule_probs :: Tnsr '[num_holes, rules] = UnsafeMkTensor . stack' 0 $ toDynamic . getGold <$> findHolesExpr ppt
            where getGold (getter, _setter) = let
                        gold_rule_idx :: Int = (ruleIdxs !) . nodeRule . getter $ task_fn
                        rule_probs :: Tnsr '[rules] = UnsafeMkTensor . D.asTensor . setAt gold_rule_idx 1.0 $ zeroes
                    in rule_probs
    let loss :: Tnsr '[num_holes, rules] = abs $ hole_expansion_probs `sub` gold_rule_probs

    return (num_holes-1, ppt', toDynamic loss)

-- -- for each task fn I think the stats I may want include:
-- -- total samples, number correct/wrong, ratio correct/wrong,
-- -- then the number / ratio over all task fns.
-- errorCount
--   :: forall batchSize outputFeatures device
--    . ( KnownNat batchSize
--      , KnownNat outputFeatures
--      , SumDTypeIsValid device 'D.Bool
--      , ComparisonDTypeIsValid device 'D.Int64
--      )
--   => Tensor Dev 'D.Float '[batchSize, outputFeatures] -- ^ prediction
--   -> Tensor Dev 'D.Int64 '[batchSize] -- ^ target
--   -> Tensor Dev 'D.Float '[]
-- errorCount prediction = Torch.Typed.Tensor.toDType @D.Float . sumAll . ne (argmax @1 @DropDim prediction)

train :: forall m batchSize rules t n_train n_validation n_test . (KnownNat m, KnownNat batchSize, KnownNat rules, KnownNat t, KnownNat n_train, KnownNat n_validation, KnownNat n_test) => SynthesizerConfig -> TaskFnDataset -> IO ()  -- Interpreter ()
train SynthesizerConfig{..} TaskFnDataset{..} = do
    let rules :: Int = natValI @rules

    -- GENERAL

    -- datasets
    let set_list = untuple3 datasets
    forM_ (zip ["train", "validation", "test"] set_list) $ \(k, dataset) -> do
        putStrLn $ k <> ": " <> show (length dataset)
    -- TODO: use validation
    let [train_set, validation_set, test_set] :: [[Expr]] = set_list
    let all_sets :: [Expr] = join set_list
    -- let [n_train, n_validation, n_test] :: [Int] = assertP (== [natValI @n_train, natValI @n_validation, natValI @n_test]) $ length <$> set_list

    -- misc
    -- device <- getDevice
    -- let device :: D.Device = D.Device D.CPU 0
    let stdGen :: StdGen = mkStdGen seed
    -- torchGen :: Generator <- D.mkGenerator device $ fromIntegral seed
    -- ATen.manual_seed_L seed
    -- let dsl = blockAsts
    -- block_fn_types :: HashMap String Tp <- interpretUnsafe $ mapM exprType dsl
    -- assert our (hole-variant) blocks match their static length
    -- variants :: [(String, Expr)] <- interpretUnsafe $ dslVariants dsl
    let variants :: [(String, Expr)] = assertP ((== rules) . length) $ exprBlocks
    -- (symbol_emb, rule_emb) <- initEmbeds device seed $ length variants
    let lr :: Tnsr '[] = UnsafeMkTensor . D.asTensor $ learningRate
    -- let lr :: D.Tensor = D.asTensor learningRate

    -- pre-calculate DSL stuff
    task_expr_types :: HashMap Expr Tp <-
            interpretUnsafe $ exprType `fromKeysM` all_sets
    let task_type_ins :: HashMap Expr (HashMap [Tp] [Expr]) =
            fmap (fmap fst) <$> fnInTypeInstanceOutputs
    -- combine i/o lists across type instances
    let task_io_pairs :: HashMap Expr [(Expr, Either String Expr)] =
            join . elems <$> fnInTypeInstanceOutputs
    -- then take their outputs
    let task_outputs :: HashMap Expr [Either String Expr] =
            fmap snd <$> task_io_pairs
    let ruleIdxs :: HashMap String Int = indexList $ fst <$> variants
    -- -- TODO: by task fn create a hashmap from holes to expansion vectors. this implies hashable lenses tho...?
    -- taskFnExpansion :: HashMap Expr (HashMap HoleLens? (Tnsr '[rules])) = fromList . flip fromKeys all_sets $ \expr -> fromList $ (\hole -> (_holeLens?, rulesTensor?)) <$> findHolesExpr expr
    -- TODO: check if variants actually has holed versions as string keys. if not this'll just go boom.
    let variantMap :: HashMap String Expr = fromList variants
    let ios :: [(Expr, Either String Expr)] =
            join . elems $ join . elems <$> fnInTypeInstanceOutputs
    let longest_string :: Int = assertP (== natValI @t) . maximum $ length <$> fmap (pp . fst) ios <> fmap (pp_ . snd) ios
    putStrLn $ "longest allowed i/o string length: " <> show longest_string

    -- MODELS
    let encoder_spec :: BaselineMLPEncoderSpec = BaselineMLPEncoderSpec max_char h0 h1 $ dirs * Enc.h
    let r3nn_spec :: R3NNSpec m rules = initR3nn @m @rules @t variants batchSize
    -- init_r3nn_model :: R3NN m rules <- initR3nn gen variants batchSize
    -- init_r3nn_model :: R3NN m rules <- initR3nn seed variants batchSize
    -- init_r3nn_model :: R3NN m rules <- initR3nn @m @rules @t variants batchSize
    init_model :: NSPS m rules <- A.sample $ NSPSSpec @m @rules encoder_spec r3nn_spec
    --  :: Adam momenta1
    -- let init_enc_optim  = mkAdam 0 0.9 0.999 $ flattenParameters init_enc_model
    -- let init_r3nn_optim = mkAdam 0 0.9 0.999 $ flattenParameters init_r3nn_model
    -- let init_enc_optim  = default_optim
    -- let init_r3nn_optim = default_optim
    let init_optim :: D.GD = default_optim
    -- let init = (init_model, init_optim)

    foldLoop_ (stdGen, init_model, init_optim) numEpochs $ \(gen, model_, optim_) epoch -> do
        let (train_set', gen') = fisherYates gen train_set    -- shuffle

        -- TRAIN LOOP

        -- -- TODO: replace with batched loop
        -- let model :: R3NN m rules = init_model
        -- let optim = init_optim

        -- corrects :: Tnsr '[n_train] <- fmap (UnsafeMkTensor . F.toDType D.Float . D.asTensor) $ 
        -- Tnsr '[*]?
        -- TODO: let loss :: D.Tensor = crossEntropy target dim input
        let foldrM_ x xs f = foldrM f x xs
        (train_losses, model', optim') :: ([D.Tensor], NSPS m rules, D.GD) <- foldrM_ ([], model_, optim_) train_set' $ \ task_fn (train_losses, model, optim) -> do
            putStrLn $ "task_fn: " <> pp task_fn

            let taskType :: Tp = task_expr_types                ! task_fn
            -- let type_ins :: HashMap [Tp] [Expr] = task_type_ins ! task_fn
            let target_io_pairs :: [(Expr, Either String Expr)] =
                    task_io_pairs                               ! task_fn
            -- let target_outputs :: [Either String Expr] =
            --         task_outputs                                ! task_fn

            io_feats :: Tnsr '[batchSize, 2 * Dirs * Enc.H * t] <- baselineMLPEncoder (encoder model) target_io_pairs
            (_zero, _program, losses) :: (Int, Expr, [D.Tensor]) <- let
                    --  :: forall num_holes x . (Int, Expr) -> IO (Int, Expr)
                    fill = \(_num_holes, ppt, losses) ->
                            fmap (\(n, p, loss) -> (n, p, loss : losses)) $ join $ fillHoleTrain variantMap ruleIdxs task_fn ppt <$> runR3nn @t (r3nn model) ppt io_feats
                    in while (\(num_holes, _, _) -> num_holes > 0) fill (1 :: Int, skeleton taskType, [])     -- hasHoles
            -- return $ pp task_fn == pp program
            let loss :: Tnsr '[] = UnsafeMkTensor . F.mean . F.cat 0 $ losses
            -- TODO: do once for each mini-batch / fn?
            (newParam, optim') <- D.runStep model optim (toDynamic loss) $ toDynamic lr
            let model' = A.replaceParameters model newParam
            return (toDynamic loss : train_losses, model', optim')

        -- aggregating over task fns, which in turn had separately aggregated over any holes encountered across the different synthesis steps (so multiple times for a hole encountered across various PPTs along the way). this is fair, right?
        let loss_train :: Tnsr '[] = UnsafeMkTensor . F.mean . stack' 0 $ train_losses

        -- TEST

        -- let tensor :: Tnsr '[n_test, *] = ? $ test_set
        -- let n :: Int = (shape tensor !! 0)  -- n_test?
        -- let tensor' = snd . shuffle 0 gen $ tensor
        -- let batches :: [Tnsr '[batchSize, *]] = batchTensor batchSize tensor'
        -- let meanLoss' :: Tnsr '[] = batchStatistic sum (/ realToFrac n) batches  -- first meanDim sampleDim but for variable lengths

        -- (loss_test, err_test) <- foldLoop (0,0) test_iters $ \(org_loss, org_err) i -> do
        --     (loss, err) <- computeLossAndErrorCount @batchSize (forward enc_model' False) i test_set
        --     return (org_loss + toFloat loss, org_err + toFloat err)

        -- forM_ test_set $ \task_fn -> do

        -- TODO: figure out if I can mini-batch my loop...
        -- let task_batches = batchList batchSize train_set'
        -- forM task_batches $ \batch -> do
        -- forM batch $ \task_fn -> do
        -- train_set' `forM_` \task_fn -> do

        errors_test :: Tnsr '[n_test] <- fmap (UnsafeMkTensor . F.toDType D.Float . D.asTensor) $ forM test_set $ \task_fn -> do
            let taskType :: Tp = task_expr_types                ! task_fn
            let type_ins :: HashMap [Tp] [Expr] = task_type_ins ! task_fn
            let target_io_pairs :: [(Expr, Either String Expr)] =
                    task_io_pairs                               ! task_fn
            let target_outputs :: [Either String Expr] =
                    task_outputs                                ! task_fn

            -- forM_ target_io_pairs $ \io_pair -> do
            -- let io_batches = batchList batchSize target_io_pairs
            -- forM_ io_batches $ \batch -> do
            io_feats :: Tnsr '[batchSize, 2 * Dirs * Enc.H * t] <- baselineMLPEncoder (encoder model') target_io_pairs
            -- putStrLn . show . shape' $ io_feats

            -- sample for best of 100 predictions
            sample_matches :: [Bool] <- replicateM bestOf $ do
                -- TODO: use these ExprTypeSig type annotations
                -- TODO: split io_feats and taskType based on param type instance combo 
                (_zero, program) :: (Int, Expr) <- let
                        --  :: forall num_holes x . (Int, Expr) -> IO (Int, Expr)
                        fill = \(_num_holes, ppt) ->
                                join $ predictHole variants ppt <$> runR3nn @t (r3nn model') ppt io_feats
                        in while ((> 0) . fst) fill (1 :: Int, skeleton taskType)     -- hasHoles

                prediction_type_ios :: HashMap [Tp] [(Expr, Either String Expr)] <- let
                        -- crash_on_error=False is slower but lets me check if it compiles
                        compileInput :: [Expr] -> IO [(Expr, Either String Expr)] = \ins -> let
                                n :: Int = length $ unTuple $ ins !! 0
                            in interpretUnsafe $ fnIoPairs False n program $ list ins
                    in compileInput `mapM` type_ins
                let prediction_io_pairs :: [(Expr, Either String Expr)] =
                        join . elems $ prediction_type_ios
                let prediction_outputs :: [Either String Expr] = snd <$> prediction_io_pairs
                let output_matches :: [Bool] = uncurry (==) . mapTuple pp_ <$> target_outputs `zip` prediction_outputs
                let outputs_match :: Bool = and output_matches
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
            -- let score :: Float = max sample_matches
            return $ not best_works

        -- let error_rate_test :: Tnsr '[] = mean errors_test
        let error_rate_test :: Tnsr '[] = asUntyped F.mean errors_test

        -- REST

        putStrLn
            $  "Epoch: "
            <> show epoch
            <> ". Train loss: "
            <> show loss_train
            -- statistic (zeros :: Tnsr '[0, ?]) sufficientStatistic summarizer test_set
            -- wait, reducing separate tensor rows feels antithetical to a parallel lib
            -- <> ". Test loss: "
            -- <> show (loss_test / realToFrac n_test)
            <> ". Test error-rate: "
            <> show error_rate_test
        
        D.save (D.toDependent <$> A.flattenParameters model') modelPath
        return (gen', model', optim')
