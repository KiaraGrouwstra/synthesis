{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Synthesis.Synthesizer.NSPS (
    train,
) where

import System.Random (StdGen, mkStdGen)
import Data.List (null)
import Data.HashMap.Lazy (HashMap, (!), elems)
import Control.Monad (join, void, replicateM, foldM, forM, forM_)
import Control.Arrow ((&&&))
import Language.Haskell.Interpreter (Interpreter, liftIO)

import           GHC.Exts
import           GHC.TypeNats (KnownNat, Nat, Mod, Div, type (*), type (+), type (-))
import           Torch.Random (Generator)
import           Torch.Functional.Internal (gather)
import qualified Torch.DType                   as D
import qualified Torch.Tensor                  as D
import qualified Torch.Device                  as D
import qualified Torch.Random                  as D
import qualified Torch.Optim                   as D
import qualified Torch.Serialize               as D
import qualified Torch.Autograd                as D
import qualified Torch.Functional              as F
import qualified Torch.NN                      as A
import           Torch.HList
import           Torch.Typed.Aux
import           Torch.TensorOptions
import           Torch.Typed.Tensor
import           Torch.Typed.Parameter
import           Torch.Typed.Factories
import           Torch.Typed.Optim
import           Torch.Typed.Functional
import           Torch.Typed.Serialize

import           Synthesis.Types
import           Synthesis.Orphanage ()
import           Synthesis.Data hiding (GenerationConfig(..))
import           Synthesis.Utility
import           Synthesis.Ast
import           Synthesis.Configs
import           Synthesis.Blocks
import           Synthesis.FindHoles
import           Synthesis.Generation
import           Synthesis.Hint
import           Synthesis.Types
import           Synthesis.TypeGen
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import qualified Synthesis.Synthesizer.Encoder as Enc
import           Synthesis.Synthesizer.R3NN

-- dummy values to trick the compiler
type NumHoles = 123

foldLoop :: forall a b m . (Num a, Enum a, Monad m) => b -> a -> (b -> a -> m b) -> m b
foldLoop x count block = foldM block x ([1 .. count] :: [a])

foldLoop_ :: forall a b m . (Num a, Enum a, Monad m) => b -> a -> (b -> a -> m b) -> m ()
foldLoop_ = ((void .) .) . foldLoop

-- | initialize a R3NN model + its optimizer
initR3nn :: forall m rules t . (KnownNat m, KnownNat rules, KnownNat t) => [(String, Expr)] -> Int -> IO (R3NN m rules)
initR3nn variants batch_size = do
    -- MODELS
    let t :: Int = natValI @t
    let variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
            where variantInt :: Expr -> (String, Int) = (appRule &&& length) . fnAppNodes
    join . return . A.sample $ R3NNSpec @m @rules
        variant_sizes
        -- left
        hiddenFeatures0
        hiddenFeatures1
        -- right
        hiddenFeatures0
        hiddenFeatures1
        -- condition MLP
        -- TODO: can I really cram all that back into just M?
        (m + batch_size * 2 * dirs * Enc.h * t)
        h0
        h1
        m
        -- score MLP
        m
        h0
        h1
        m

predict :: forall t num_holes batchSize m rules . (KnownNat t, KnownNat num_holes) => [(String, Expr)] -> R3NN m rules -> Tnsr '[batchSize, 2 * Dirs * Enc.H * t] -> Expr -> IO Expr
predict variants r3nn_model io_feats ppt = do
    let hole_lenses = findHolesExpr ppt
    let num_holes :: Int = case length hole_lenses of
            0 -> error "program complete, PT is no longer a PPT!"
            x -> x

    hole_expansion_probs <- r3nn @t r3nn_model ppt io_feats
    -- putStrLn . show . shape' $ hole_expansion_probs

    let (hole_dim, rule_dim) :: (Int, Int) = (0, 1)
    let rule_idx_by_hole :: Tensor Dev 'D.Int64 '[num_holes] =
            asUntyped (F.argmax (F.Dim rule_dim) F.RemoveDim) hole_expansion_probs
    -- putStrLn . show . shape' $ rule_idx_by_hole
    let best_prob_by_hole :: Tnsr '[num_holes] =
            UnsafeMkTensor $ F.squeezeAll $ gather
                (toDynamic hole_expansion_probs)
                rule_dim
                (D.reshape [num_holes, 1] $ toDynamic rule_idx_by_hole)
                False
    -- putStrLn . show . shape' $ best_prob_by_hole
    let hole_idx :: Int = D.asValue $ F.argmax (F.Dim 0) F.RemoveDim $ toDynamic best_prob_by_hole
    let rule_idx :: Int = D.asValue $ D.select (toDynamic rule_idx_by_hole) 0 hole_idx
    let estimated_probability :: Float = D.asValue $
            D.select (D.select (toDynamic hole_expansion_probs) hole_dim hole_idx) 0 rule_idx

    -- order rules: comes from symbol_expansions_emb, which is just randomly assigned,
    -- so I don't need to match with anything,
    -- and can just arbitrarily associate this with any deterministic order of block variants
    let (rule_str, rule_expr) :: (String, Expr) = variants !! rule_idx

    -- order NumHoles: node_embs. without SrcSpanInfo Expr isn't uniquely Hashable,
    -- so grab hole lenses by `findHolesExpr` and ensure `node_embs` follows the same order.
    let (hole_getter, hole_setter) :: (Expr -> Expr, Expr -> Expr -> Expr) =
            findHolesExpr ppt !! hole_idx
    let ppt' :: Expr = hole_setter ppt rule_expr
    putStrLn . show $ (hole_idx, rule_idx, estimated_probability, rule_str, pp ppt')
    return ppt'

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
--   => Tensor device 'D.Float '[batchSize, outputFeatures] -- ^ prediction
--   -> Tensor device 'D.Int64 '[batchSize] -- ^ target
--   -> Tensor device 'D.Float '[]
-- errorCount prediction = Torch.Typed.Tensor.toDType @D.Float . sumAll . ne (argmax @1 @DropDim prediction)

train :: forall m batchSize rules t . (KnownNat m, KnownNat batchSize, KnownNat rules, KnownNat t) => SynthesizerConfig -> TaskFnDataset -> IO ()  -- Interpreter ()
train SynthesizerConfig{..} TaskFnDataset{..} = do

    -- GENERAL

    -- datasets
    let set_list = untuple3 datasets
    forM_ (zip ["train", "validation", "test"] set_list) $ \(k, dataset) -> do
        putStrLn $ k <> ": " <> show (length dataset)
    let [train_set, validation_set, test_set] :: [[Expr]] = set_list
    let all_sets :: [Expr] = join set_list
    let [n_train, n_validation, n_test] :: [Int] = length <$> set_list

    -- misc
    -- device <- getDevice
    let device :: D.Device = D.Device D.CPU 0
    let stdGen :: StdGen = mkStdGen seed
    torchGen :: Generator <- D.mkGenerator device $ fromIntegral seed
    -- ATen.manual_seed_L seed
    let dsl = blockAsts
    block_fn_types :: HashMap String Tp <- interpretUnsafe $ mapM exprType dsl
    -- variants :: [(String, Expr)] <- interpretUnsafe $ dslVariants dsl
    let variants :: [(String, Expr)] = expr_blocks
    -- (symbol_emb, symbol_expansions_emb) <- initEmbeds device seed $ length variants
    let lr :: Tnsr '[] = UnsafeMkTensor . D.asTensor $ learningRate

    -- pre-calculate DSL crap
    task_expr_types :: HashMap Expr Tp <-
            interpretUnsafe $ exprType `fromKeysM` all_sets
    let task_type_ins :: HashMap Expr (HashMap [Tp] [Expr]) =
            fmap (fmap fst) <$> fn_in_type_instance_outputs
    -- combine i/o lists across type instances
    let task_io_pairs :: HashMap Expr [(Expr, Either String Expr)] =
            join . elems <$> fn_in_type_instance_outputs
    -- then take their outputs
    let task_outputs :: HashMap Expr [Either String Expr] =
            fmap snd <$> task_io_pairs

    -- MODELS
    init_enc_model :: BaselineMLPEncoder <- A.sample $ BaselineMLPEncoderSpec max_char h0 h1 $ dirs * Enc.h
    -- init_r3nn_model :: R3NN m rules <- initR3nn gen variants batchSize
    -- init_r3nn_model :: R3NN m rules <- initR3nn seed variants batchSize
    init_r3nn_model :: R3NN m rules <- initR3nn @m @rules @t variants batchSize
    --  :: Adam momenta1
    -- let init_enc_optim  = mkAdam 0 0.9 0.999 $ flattenParameters init_enc_model
    -- let init_r3nn_optim = mkAdam 0 0.9 0.999 $ flattenParameters init_r3nn_model
    let init_enc_optim  = default_optim
    let init_r3nn_optim = default_optim
    let r3nn_init = (init_r3nn_model, init_r3nn_optim)

    -- TRAIN LOOP

    -- TODO: replace with batched loop
    let r3nn_model :: R3NN m rules = init_r3nn_model
    let r3nn_optim = init_r3nn_optim

    foldLoop_ (stdGen, init_enc_model, init_enc_optim) numEpochs $ \(gen, enc_model, enc_optim) epoch -> do
        let (train_set', gen') = fisherYates gen train_set    -- shuffle

        -- TODO: after I've figured out how to do runStep without a gradient to parameters, figure out if I can mini-batch my loop...
        -- let task_batches = batchList batchSize train_set'
        -- forM task_batches $ \batch -> do
        -- forM batch $ \task_fn -> do
        corrects :: Tnsr '[n_train] <- fmap (UnsafeMkTensor . F.toDType D.Float . D.asTensor) $ forM train_set' $ \task_fn -> do
            let taskType :: Tp = task_expr_types                ! task_fn
            let type_ins :: HashMap [Tp] [Expr] = task_type_ins ! task_fn
            let target_io_pairs :: [(Expr, Either String Expr)] =
                    task_io_pairs                               ! task_fn
            let target_outputs :: [Either String Expr] =
                    task_outputs                                ! task_fn

            -- forM_ target_io_pairs $ \io_pair -> do
            -- let io_batches = batchList batchSize target_io_pairs
            -- forM_ io_batches $ \batch -> do
            io_feats :: Tnsr '[batchSize, 2 * Dirs * Enc.H * t] <- baselineMLPEncoder enc_model target_io_pairs
            -- putStrLn . show . shape' $ io_feats

            -- sample for best of 100 predictions
            sample_matches :: [Bool] <- replicateM bestOf $ do
                -- TODO: use these ExprTypeSig type annotations
                -- TODO: split io_feats and taskType based on param type instance combo 
                -- @NumHoles: fool the compiler by supplying a static dummy type
                program :: Expr <- while hasHoles (predict @t @NumHoles variants r3nn_model io_feats) $ skeleton taskType

                prediction_type_ios :: HashMap [Tp] [(Expr, Either String Expr)] <- let
                        -- crashOnError=False is slower but lets me check if it compiles
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
            return best_works

        -- TODO: evaluate this MSE loss?
        -- cf.: soft margin loss, sigmoid, binary cross entropy (with logits).
        -- for mean score across i/o samples,
        -- unfortunately the built-in loss fns reduce over all dims, 
        -- which doesn't really work for non-associative reductions like mean.
        -- there take into account the variable number of samples per task fn.
        -- later I could try fancier stuff like reweighing to focus on hard tasks.
        let error :: Tnsr '[n_train] = onesLike corrects `sub` corrects
        let loss :: Tnsr '[] = meanDim @0 . square $ error

        -- TODO: how am I to backprop over something that hasn't been tensors all the way?
        -- runStep  enc_model  enc_optim loss lr
        -- runStep r3nn_model r3nn_optim loss lr
        D.runStep  enc_model  enc_optim (toDynamic loss) (toDynamic lr)
        D.runStep r3nn_model r3nn_optim (toDynamic loss) (toDynamic lr)
        -- TODO: does it even make sense to propagate a loss to two distinct models?!
        -- TODO: batch this loop so it can learn in-between:
        -- ( enc_model',  enc_optim') <- foldLoop (enc_model, enc_optim) train_iters $ \(model, optim) i -> do
        -- (r3nn_model', r3nn_optim') <- foldLoop r3nn_init numEpochs $ \(r3nn_model, r3nnOptim) epoch -> do

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

        -- REST

        putStrLn
            $  "Epoch: "
            <> show epoch
            <> ". Train loss: "
            <> show loss
            -- TODO: use `statistic` for these to stitch together the + and / into one mean statistic?
            -- statistic (zeros :: Tnsr '[0, ?]) sufficientStatistic summarizer test_set
            -- wait, reducing separate tensor rows feels antithetical to a parallel lib
            -- honestly just stitch up the batches and directly call the native measure?
            -- <> ". Test loss: "
            -- <> show (loss_test / realToFrac n_test)
            -- <> ". Test error-rate: "
            -- <> show ( err_test / realToFrac n_test)
        
        -- save (hmap' ToDependent . flattenParameters $ enc_model) modelPath  -- enc_model'
        D.save (D.toDependent <$> A.flattenParameters enc_model) modelPath  -- enc_model'
        return (gen', enc_model, enc_optim)        -- enc_model', enc_optim'
