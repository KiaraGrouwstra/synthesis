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
import           Data.Int                     (Int64)
import           Data.Maybe                   (isNothing)
import           Data.Either                  (fromRight, isRight)
import           Data.Functor                 (void, (<&>))
import           Data.Bifunctor               (first, second)
import           Data.HashMap.Lazy            (HashMap, empty, insert, singleton, (!), keys, fromList, size)
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
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.NSPS
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical  as Categorical
import           Synthesis.Synthesizer.Params

import           Spec.Synthesizer.Types

type Device = Cpu

nsps ∷ Spec
nsps = parallel $ let
        dropOut :: Double = 0.0
        dsl = fmap parseExpr
                $ insert "nil" "[]"
                $ insert "not" "not"
                $ singleton "true" "True"
        -- expr_blocks :: [(String, Expr)] <- interpretUnsafe $ dslVariants dsl
        expr_blocks :: [(String, Expr)] = second parseExpr <$> [("nil", "nil"), ("true", "true"), ("not", "not"), ("not", "not (undefined :: Bool)")]
        lr = D.asTensor (0.01 :: Float)
    in do

    it "LstmEncoder" $ do
        -- io_pairs for task fn `trues :: Int -> String`
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "\"0\"")), (parseExpr "1", Right (parseExpr "\"1\"")), (parseExpr "2", Right (parseExpr "\"2\""))]
        let charMap :: HashMap Char Int = mkCharMap io_pairs
        -- putStrLn $ show (size charMap + 1)
        enc_model :: LstmEncoder MaxStringLength' EncoderBatch' MaxChar' <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        io_feats :: Tensor Device 'D.Float '[EncoderBatch', 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model charMap io_pairs
        D.shape (toDynamic io_feats) `shouldBe` [length io_pairs, 2 * dirs * h * maxStringLength']

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters enc_model
        let loss :: Tensor Device 'D.Float '[] = sumAll io_feats  -- dummy op for loss with gradient
        (newParam, optim') <- D.runStep enc_model optim (toDynamic loss) lr
        let enc_model' :: LstmEncoder MaxStringLength' EncoderBatch' MaxChar' = A.replaceParameters enc_model newParam

        io_feats' :: Tensor Device 'D.Float '[EncoderBatch', 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model' charMap io_pairs
        let loss' :: Tensor Device 'D.Float '[] = sumAll io_feats'
        toBool (loss' <. loss) `shouldBe` True

    it "R3NN" $ do
        let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        let variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "\"0\"")), (parseExpr "1", Right (parseExpr "\"1\"")), (parseExpr "2", Right (parseExpr "\"2\""))]
        let charMap :: HashMap Char Int = mkCharMap io_pairs
        enc_model :: LstmEncoder MaxStringLength' EncoderBatch' MaxChar' <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        io_feats :: Tensor Device 'D.Float '[R3nnBatch', 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model charMap io_pairs
        sampled_idxs :: D.Tensor <- liftIO $ F.toDType D.Int64 <$> D.randintIO' 0 (length io_pairs) [r3nnBatch']
        let sampled_feats :: Tensor Device 'D.Float '[R3nnBatch', MaxStringLength' * (2 * Dirs * H)] = UnsafeMkTensor $ D.indexSelect (toDynamic io_feats) 0 sampled_idxs
        r3nn_model :: R3NN M Symbols' Rules' MaxStringLength' R3nnBatch' <- A.sample $ initR3nn @M @Symbols' @Rules' @MaxStringLength' variants r3nnBatch' dropOut
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        hole_expansion_probs :: Tensor Device 'D.Float '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @R3nnBatch' r3nn_model symbolIdxs ppt sampled_feats
        D.shape (toDynamic hole_expansion_probs) `shouldBe` [numHoles, rules]

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters r3nn_model
        let loss :: Tensor Device 'D.Float '[] = patchLoss @M variant_sizes r3nn_model $ sumAll hole_expansion_probs  -- dummy op for loss with gradient
        (newParam, optim') <- D.runStep r3nn_model optim (toDynamic loss) lr
        let r3nn_model' :: R3NN M Symbols' Rules' MaxStringLength' R3nnBatch' = A.replaceParameters r3nn_model newParam
        hole_expansion_probs' :: Tensor Device 'D.Float '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @R3nnBatch' r3nn_model' symbolIdxs ppt sampled_feats
        let loss' :: Tensor Device 'D.Float '[] = patchLoss @M variant_sizes r3nn_model' $ sumAll hole_expansion_probs'
        toBool (loss' <. loss) `shouldBe` True

    it "predictHole" $ do
        let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        -- io_pairs for task fn `trues :: Int -> String`
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "\"0\"")), (parseExpr "1", Right (parseExpr "\"1\"")), (parseExpr "2", Right (parseExpr "\"2\""))]
        let charMap :: HashMap Char Int = mkCharMap io_pairs
        enc_model :: LstmEncoder MaxStringLength' EncoderBatch' MaxChar' <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        --  :: Tensor Device 'D.Float '[n, 2 * Dirs * H * MaxStringLength']
        io_feats <- lstmEncoder enc_model charMap io_pairs
        sampled_idxs :: D.Tensor <- liftIO $ F.toDType D.Int64 <$> D.randintIO' 0 (length io_pairs) [r3nnBatch']
        let sampled_feats :: Tensor Device 'D.Float '[R3nnBatch', MaxStringLength' * (2 * Dirs * H)] = UnsafeMkTensor $ D.indexSelect (toDynamic io_feats) 0 sampled_idxs
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        r3nn_model :: R3NN M Symbols' Rules' MaxStringLength' R3nnBatch' <- A.sample $ initR3nn @M @Symbols' @Rules' @MaxStringLength' variants r3nnBatch' dropOut
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        hole_expansion_probs :: Tensor Device 'D.Float '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @R3nnBatch' r3nn_model symbolIdxs ppt sampled_feats
        (ppt', _used') <- predictHole variants ppt (Set.singleton "not") hole_expansion_probs
        pp ppt' `shouldNotBe` pp ppt

    it "superviseHole" $ do
        let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        let variantMap :: HashMap String Expr = fromList variants
        let task_fn :: Expr = parseExpr "not (not (true))"
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        ppt' :: Expr <- superviseHole variantMap numHoles task_fn ppt
        pp ppt' `shouldBe` pp task_fn

    it "fillHoleTrain" $ do
        let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        let variantMap :: HashMap String Expr = fromList variants
        let task_fn :: Expr = parseExpr "not (not (true))"
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        let ruleIdxs :: HashMap String Int = indexList $ fst <$> variants
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "\"0\"")), (parseExpr "1", Right (parseExpr "\"1\"")), (parseExpr "2", Right (parseExpr "\"2\""))]
        let charMap :: HashMap Char Int = mkCharMap io_pairs
        enc_model :: LstmEncoder MaxStringLength' EncoderBatch' MaxChar' <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        --  :: Tensor Device 'D.Float '[n, 2 * Dirs * H * MaxStringLength']
        io_feats <- lstmEncoder enc_model charMap io_pairs
        sampled_idxs :: D.Tensor <- liftIO $ F.toDType D.Int64 <$> D.randintIO' 0 (length io_pairs) [r3nnBatch']
        let sampled_feats :: Tensor Device 'D.Float '[R3nnBatch', MaxStringLength' * (2 * Dirs * H)] = UnsafeMkTensor $ D.indexSelect (toDynamic io_feats) 0 sampled_idxs
        r3nn_model :: R3NN M Symbols' Rules' MaxStringLength' R3nnBatch' <- A.sample $ initR3nn @M @Symbols' @Rules' @MaxStringLength' variants r3nnBatch' dropOut
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        hole_expansion_probs :: Tensor Device 'D.Float '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @R3nnBatch' r3nn_model symbolIdxs ppt sampled_feats
        (task_fn', gold) :: (Expr, Tensor Device 'D.Float '[NumHoles']) <- fillHoleTrain variantMap ruleIdxs task_fn ppt hole_expansion_probs
        pp task_fn' `shouldBe` pp task_fn
        D.shape (toDynamic gold) `shouldBe` [numHoles]
