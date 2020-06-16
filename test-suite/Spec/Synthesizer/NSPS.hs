{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Synthesizer.NSPS (module Spec.Synthesizer.NSPS) where

import           Test.Tasty                   (TestTree, defaultMain, testGroup)
import           Test.HUnit.Base              (Test (..))
import           Test.HUnit.Text              (runTestTT)
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit             ((@?=))

import           Prelude                      hiding (abs, all)
import           Control.Exception            (SomeException, try, evaluate)
import           Control.Monad                (join)
import           Data.Int                     (Int64)
import           Data.Maybe                   (isNothing)
import           Data.Either                  (fromRight, isRight)
import           Data.Functor                 (void, (<&>))
import           Data.Bifunctor               (first, second)
import           Data.HashMap.Lazy            (HashMap, empty, insert, singleton, (!), keys, elems, fromList, size, filterWithKey)
import qualified Data.Set as Set
import           System.Random                (StdGen, mkStdGen)
import           System.Timeout               (timeout)
import           Language.Haskell.Interpreter (as, interpret, liftIO, typeChecks, typeChecksWithDetails)
import           Util                         (fstOf3)
import           Language.Haskell.Interpreter

import           GHC.TypeNats
import           Torch.Typed.Functional
import qualified Torch.Tensor                  as D
import qualified Torch.TensorFactories         as D
import qualified Torch.DType                   as D
import qualified Torch.Optim                   as D
import qualified Torch.Autograd                as D
import qualified Torch.Functional.Internal     as I
import qualified Torch.Functional              as F
import qualified Torch.NN                      as A
import           Torch.Typed.Aux
import           Torch.Typed.Tensor
import           Torch.Typed.Factories
import           Torch.Typed.Optim
import           Torch.Typed.Parameter
import           Torch.Typed.NN
import           Torch.Typed.NN.Recurrent.LSTM

import           Synthesis.Ast
import           Synthesis.Configs
import           Synthesis.Blocks
import           Synthesis.FindHoles
import           Synthesis.Generation
import           Synthesis.Hint
import           Synthesis.Types
import           Synthesis.TypeGen
import           Synthesis.Data
import           Synthesis.Utility
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import           Synthesis.Synthesizer.TypeEncoder
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.NSPS
import           Synthesis.Synthesizer.Train
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical  as Categorical
import           Synthesis.Synthesizer.Params

import           Spec.Synthesizer.Types

type Device = Cpu

nsps ∷ Spec
nsps = parallel $ let
        int_ = tyCon "Int"
        str = tyCon "String"
        dropOut :: Double = 0.0
        -- tp_io_pairs for task fn `trues :: Int -> String`
        tp_io_pairs :: HashMap (Tp, Tp) [(Expr, Either String Expr)] = singleton (int_, str) [(parseExpr "0", Right (parseExpr "\"0\"")), (parseExpr "1", Right (parseExpr "\"1\"")), (parseExpr "2", Right (parseExpr "\"2\""))]
        io_pairs :: [(Expr, Either String Expr)] = join . elems $ tp_io_pairs
        charMap :: HashMap Char Int = mkCharMap [tp_io_pairs]
        dsl = fmap parseExpr
                $ insert "nil" "[]"
                $ insert "not" "not"
                $ singleton "true" "True"
        -- expr_blocks :: [(String, Expr)] <- interpretUnsafe $ dslVariants dsl
        expr_blocks :: [(String, Expr)] = second parseExpr <$> [("nil", "nil"), ("true", "true"), ("not", "not"), ("not", "not (undefined :: Bool)")]
        variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        lr = D.asTensor (0.01 :: Float)
        -- dsl' = filterWithKey (\k v -> k /= pp v) dsl
        -- variantTypes :: [Tp] <- interpretUnsafe $ (exprType . letIn dsl' . snd) `mapM` variants
        variantTypes :: [Tp] = parseType <$> ["[a]", "Bool", "Bool -> Bool", "Bool"]
        ruleCharMap :: HashMap Char Int = indexChars $ pp <$> variantTypes
        type_encoder_spec :: TypeEncoderSpec Device MaxStringLength MaxChar M =
                TypeEncoderSpec ruleCharMap $ LSTMSpec $ DropoutSpec dropOut
    in do

    it "LstmEncoder" $ do
        -- putStrLn . show $ size charMap + 1
        enc_model :: LstmEncoder Device MaxStringLength EncoderBatch' MaxChar H FeatMult <- A.sample $ LstmEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropOut
        io_feats :: Tensor Device 'D.Float '[R3nnBatch', 2 * FeatMult * Dirs * H * MaxStringLength] <- sampleTensor @0 @R3nnBatch' (length io_pairs) . toDynamic $ lstmEncoder enc_model tp_io_pairs
        D.shape (toDynamic io_feats) `shouldBe` [natValI @R3nnBatch', natValI @(2 * FeatMult * Dirs * H * MaxStringLength)]

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters enc_model
        let loss :: Tensor Device 'D.Float '[] = sumAll io_feats  -- dummy op for loss with gradient
        (newParam, optim') <- D.runStep enc_model optim (toDynamic loss) lr
        let enc_model' :: LstmEncoder Device MaxStringLength EncoderBatch' MaxChar H FeatMult = A.replaceParameters enc_model newParam

        io_feats' :: Tensor Device 'D.Float '[R3nnBatch', 2 * FeatMult * Dirs * H * MaxStringLength] <- sampleTensor @0 @R3nnBatch' (length io_pairs) . toDynamic $ lstmEncoder enc_model' tp_io_pairs
        let loss' :: Tensor Device 'D.Float '[] = sumAll io_feats'
        toBool (loss' <. loss) `shouldBe` True

    it "TypeEncoder" $ do
        rule_encoder <- A.sample type_encoder_spec
        let rule_tp_emb :: Tensor Device 'D.Float '[Rules, MaxStringLength * M] =
                typeEncoder @Rules rule_encoder variantTypes
        D.shape (toDynamic rule_tp_emb) `shouldBe` [natValI @Rules, natValI @(MaxStringLength * M)]

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters rule_encoder
        let loss :: Tensor Device 'D.Float '[] = sumAll rule_tp_emb  -- dummy op for loss with gradient
        (newParam, optim') <- D.runStep rule_encoder optim (toDynamic loss) lr
        let rule_encoder' = A.replaceParameters rule_encoder newParam

        let rule_tp_emb' :: Tensor Device 'D.Float '[Rules, MaxStringLength * M] =
                typeEncoder @Rules rule_encoder' variantTypes
        let loss' :: Tensor Device 'D.Float '[] = sumAll rule_tp_emb'
        toBool (loss' <. loss) `shouldBe` True

    it "R3NN" $ do
        let variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
        enc_model :: LstmEncoder Device MaxStringLength EncoderBatch' MaxChar H FeatMult <- A.sample $ LstmEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropOut
        sampled_feats :: Tensor device 'D.Float '[R3nnBatch', MaxStringLength * (2 * FeatMult * Dirs * H)]
                <- sampleTensor @0 @R3nnBatch' (length io_pairs) . toDynamic $ lstmEncoder enc_model tp_io_pairs
        putStrLn $ "sampled_feats: " <> show sampled_feats
        r3nn_model :: R3NN Device M Symbols Rules MaxStringLength R3nnBatch' H MaxChar FeatMult <- A.sample $ initR3nn variants r3nnBatch' dropOut ruleCharMap
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        rule_encoder <- A.sample type_encoder_spec
        -- putStrLn $ "rule_tp_emb : " <> show rule_tp_emb
        let rule_tp_emb :: Tensor Device 'D.Float '[Rules, MaxStringLength * M] =
                typeEncoder @Rules rule_encoder variantTypes
        let hole_expansion_probs :: Tensor Device 'D.Float '[NumHoles, Rules] = runR3nn r3nn_model symbolIdxs ppt rule_tp_emb sampled_feats
        D.shape (toDynamic hole_expansion_probs) `shouldBe` [numHoles, rules]

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters r3nn_model
        let loss :: Tensor Device 'D.Float '[] = patchR3nnLoss r3nn_model variant_sizes $ sumAll hole_expansion_probs  -- dummy op for loss with gradient
        (newParam, optim') <- D.runStep r3nn_model optim (toDynamic loss) lr
        let r3nn_model' :: R3NN Device M Symbols Rules MaxStringLength R3nnBatch' H MaxChar FeatMult = A.replaceParameters r3nn_model newParam
        let hole_expansion_probs' :: Tensor Device 'D.Float '[NumHoles, Rules] = runR3nn r3nn_model' symbolIdxs ppt rule_tp_emb sampled_feats
        let loss' :: Tensor Device 'D.Float '[] = patchR3nnLoss r3nn_model' variant_sizes $ sumAll hole_expansion_probs'
        toBool (loss' <. loss) `shouldBe` True

    it "predictHole" $ do
        enc_model :: LstmEncoder Device MaxStringLength EncoderBatch' MaxChar H FeatMult <- A.sample $ LstmEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropOut
        sampled_feats :: Tensor device 'D.Float '[R3nnBatch', MaxStringLength * (2 * FeatMult * Dirs * H)]
                <- sampleTensor @0 @R3nnBatch' (length io_pairs) . toDynamic $ lstmEncoder enc_model tp_io_pairs
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        r3nn_model :: R3NN Device M Symbols Rules MaxStringLength R3nnBatch' H MaxChar FeatMult <- A.sample $ initR3nn variants r3nnBatch' dropOut ruleCharMap
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        rule_encoder <- A.sample type_encoder_spec
        let rule_tp_emb :: Tensor Device 'D.Float '[Rules, MaxStringLength * M] =
                typeEncoder @Rules rule_encoder variantTypes
        let hole_expansion_probs :: Tensor Device 'D.Float '[NumHoles, Rules] = runR3nn r3nn_model symbolIdxs ppt rule_tp_emb sampled_feats
        (ppt', _used') <- predictHole variants ppt (Set.singleton "not") hole_expansion_probs
        pp ppt' `shouldNotBe` pp ppt

    it "superviseHole" $ do
        let variantMap :: HashMap String Expr = fromList variants
        let task_fn :: Expr = parseExpr "not (not (true))"
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        ppt' :: Expr <- superviseHole @Device variantMap numHoles task_fn ppt
        pp ppt' `shouldBe` pp task_fn

    it "fillHoleTrain" $ do
        let variantMap :: HashMap String Expr = fromList variants
        let task_fn :: Expr = parseExpr "not (not (true))"
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        let ruleIdxs :: HashMap String Int = indexList $ fst <$> variants
        enc_model :: LstmEncoder Device MaxStringLength EncoderBatch' MaxChar H FeatMult <- A.sample $ LstmEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropOut
        sampled_feats :: Tensor device 'D.Float '[R3nnBatch', MaxStringLength * (2 * FeatMult * Dirs * H)]
                <- sampleTensor @0 @R3nnBatch' (length io_pairs) . toDynamic $ lstmEncoder enc_model tp_io_pairs
        r3nn_model :: R3NN Device M Symbols Rules MaxStringLength R3nnBatch' H MaxChar FeatMult <- A.sample $ initR3nn variants r3nnBatch' dropOut ruleCharMap
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        rule_encoder <- A.sample type_encoder_spec
        let rule_tp_emb :: Tensor Device 'D.Float '[Rules, MaxStringLength * M] =
                typeEncoder @Rules rule_encoder variantTypes
        let hole_expansion_probs :: Tensor Device 'D.Float '[NumHoles, Rules] = runR3nn r3nn_model symbolIdxs ppt rule_tp_emb sampled_feats
        (task_fn', gold) :: (Expr, Tensor Device 'D.Float '[NumHoles]) <- fillHoleTrain variantMap ruleIdxs task_fn ppt hole_expansion_probs
        pp task_fn' `shouldBe` pp task_fn
        D.shape (toDynamic gold) `shouldBe` [numHoles]
