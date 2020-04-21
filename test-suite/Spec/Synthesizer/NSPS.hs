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
import           Data.HashMap.Lazy            (HashMap, empty, insert, singleton, (!), keys, fromList)
import qualified Data.Set
import           System.Random                (StdGen, mkStdGen)
import           System.Timeout               (timeout)
import           Language.Haskell.Interpreter (as, interpret, liftIO, typeChecks, typeChecksWithDetails)
import           Util                         (fstOf3)
import           Language.Haskell.Interpreter

import           GHC.TypeNats
import           Torch.Typed.Functional
import qualified Torch.Tensor                  as D
import qualified Torch.TensorFactories         as D
import qualified Torch.Optim                   as D
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
        -- io_pairs for task fn `trues :: Int -> [Bool]`
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "[]")), (parseExpr "1", Right (parseExpr "[True]")), (parseExpr "2", Right (parseExpr "[True, True]"))]
        enc_model :: LstmEncoder <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        io_feats :: Tnsr '[BatchSize, 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model io_pairs
        D.shape (toDynamic io_feats) `shouldBe` [batchSize, 2 * dirs * h * t]

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters enc_model
        let loss :: Tnsr '[] = sumAll io_feats  -- dummy op for loss with gradient
        (newParam, optim') <- D.runStep enc_model optim (toDynamic loss) lr
        let enc_model' :: LstmEncoder = A.replaceParameters enc_model newParam

        io_feats' :: Tnsr '[BatchSize, 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model' io_pairs
        let loss' :: Tnsr '[] = sumAll io_feats'
        putStrLn $ "LstmEncoder.loss: " <> show loss
        putStrLn $ "LstmEncoder.loss': " <> show loss'
        toBool (lt loss' loss) `shouldBe` True

    it "R3NN" $ do
        let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        let variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "[]")), (parseExpr "1", Right (parseExpr "[True]")), (parseExpr "2", Right (parseExpr "[True, True]"))]
        enc_model :: LstmEncoder <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        io_feats :: Tnsr '[BatchSize, 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model io_pairs
        r3nn_model :: R3NN M Symbols' Rules' MaxStringLength' BatchSize <- A.sample $ initR3nn @M @Symbols' @Rules' @MaxStringLength' variants batchSize dropOut
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        hole_expansion_probs :: Tnsr '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @BatchSize r3nn_model symbolIdxs ppt io_feats
        D.shape (toDynamic hole_expansion_probs) `shouldBe` [numHoles, rules]

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters r3nn_model
        let loss :: Tnsr '[] = patchLoss @M variant_sizes r3nn_model $ sumAll hole_expansion_probs  -- dummy op for loss with gradient
        (newParam, optim') <- D.runStep r3nn_model optim (toDynamic loss) lr
        let r3nn_model' :: R3NN M Symbols' Rules' MaxStringLength' BatchSize = A.replaceParameters r3nn_model newParam
        hole_expansion_probs' :: Tnsr '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @BatchSize r3nn_model' symbolIdxs ppt io_feats
        let loss' :: Tnsr '[] = patchLoss @M variant_sizes r3nn_model' $ sumAll hole_expansion_probs'
        toBool (lt loss' loss) `shouldBe` True

    it "predictHole" $ do
        let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        -- io_pairs for task fn `trues :: Int -> [Bool]`
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "[]")), (parseExpr "1", Right (parseExpr "[True]")), (parseExpr "2", Right (parseExpr "[True, True]"))]
        enc_model :: LstmEncoder <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        io_feats :: Tnsr '[BatchSize, 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model io_pairs
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        r3nn_model :: R3NN M Symbols' Rules' MaxStringLength' BatchSize <- A.sample $ initR3nn @M @Symbols' @Rules' @MaxStringLength' variants batchSize dropOut
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        hole_expansion_probs :: Tnsr '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @BatchSize r3nn_model symbolIdxs ppt io_feats
        (ppt', _used') <- predictHole variants ppt (Data.Set.singleton "not") hole_expansion_probs
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
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "[]")), (parseExpr "1", Right (parseExpr "[True]")), (parseExpr "2", Right (parseExpr "[True, True]"))]
        enc_model :: LstmEncoder <- A.sample $ LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        io_feats :: Tnsr '[BatchSize, 2 * Dirs * H * MaxStringLength'] <- lstmEncoder enc_model io_pairs
        r3nn_model :: R3NN M Symbols' Rules' MaxStringLength' BatchSize <- A.sample $ initR3nn @M @Symbols' @Rules' @MaxStringLength' variants batchSize dropOut
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        hole_expansion_probs :: Tnsr '[NumHoles', Rules'] <- runR3nn @Symbols' @M @MaxStringLength' @Rules' @BatchSize r3nn_model symbolIdxs ppt io_feats
        (task_fn', gold) :: (Expr, Tnsr '[NumHoles']) <- fillHoleTrain variantMap ruleIdxs task_fn ppt hole_expansion_probs
        pp task_fn' `shouldBe` pp task_fn
        D.shape (toDynamic gold) `shouldBe` [numHoles]
