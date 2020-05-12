{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Synthesizer.Utility (module Spec.Synthesizer.Utility) where

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
import qualified Torch.DType                   as D
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
import           Synthesis.Synthesizer.Train
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical  as Categorical
import           Synthesis.Synthesizer.Params

synth_util ∷ Spec
synth_util = parallel $ do

    it "unravelIdx" $ do
        unravelIdx (D.asTensor [ [0.3, 0.2], [0.4, 0.1 :: Float] ]) 2 `shouldBe` [1, 0]

    it "cumulative" $ do
        cumulative [1,2,3] `shouldBe` [1,3,6 :: Int]

    it "nodeRule" $ do
        nodeRule (parseExpr "f") `shouldBe` "f"
        nodeRule (parseExpr "f a b") `shouldBe` "f _ _"
        nodeRule (parseExpr "f (g c) (h d)") `shouldBe` "f _ _"

    it "fnAppNodes" $ do
        pp_ (fnAppNodes $ parseExpr "f a b") `shouldBe` "[\"f\", \"a\", \"b\"]"
        pp_ (fnAppNodes $ parseExpr "f") `shouldBe` "[\"f\"]"

    it "rotate" $ do
        rotate [10,20] `shouldBe` [[10,20,0],[0,10,20],[20,0,10]]

    -- it "rotateT" $ do
    --     let r :: Tensor Cpu 'D.Float '[2] = UnsafeMkTensor . D.asTensor $ [10.0,20.0::Float]
    --     rotateT r `shouldBe` ?

    it "categorical" $ do
        t :: Tensor Cpu 'D.Float '[2, 3] <- abs <$> randn
        x <- Distribution.sample (Categorical.fromProbs $ toDynamic t) [1]
        D.shape x `shouldBe` [1,2]

    it "sampleIdxs" $ do
        let t = D.asTensor [[0.0, 0.0], [1.0, 0.0 :: Float]]
        idxs <- sampleIdxs t
        D.asValue (foldl (\ t' idx -> D.select t' 0 idx) t idxs) `shouldBe` (1.0 :: Float)

    it "crossEntropy" $ do
        let rule_dim = 1
        let gold_rule_probs = D.asTensor [ 2 :: Int64 ]
        let hole_expansion_probs = D.asTensor [[0.2606, -7.2186e-2, 0.4544 :: Float]]
        let loss :: Tensor Cpu 'D.Float '[] = UnsafeMkTensor $ crossEntropy gold_rule_probs rule_dim hole_expansion_probs
        toFloat loss > 0.0 `shouldBe` True

    -- it "gpu" $ do
    --     putStrLn $ "availableDevices: " <> show availableDevices
    --     dev <- getDevice
    --     putStrLn $ "dev: " <> show dev
    --     let t = D.toCUDA $ D.asTensor $ [1,2,3::Int]
    --     putStrLn $ "t: " <> show t
    --     False `shouldBe` True

    it "pickIdxs" $ do
        pickIdxs [0,3,7] [0..10] `shouldBe` [0,3,7::Int]

    it "d_repeat" $ do
        D.asValue (d_repeat [2,1] (D.asTensor [[1,2],[3,4::Int]])) `shouldBe` [[1,2],[3,4],[1,2],[3,4::Int]]

    it "repeatDim" $ do
        D.asValue (repeatDim 1 2 (D.asTensor [[1,2],[3,4::Int]])) `shouldBe` [[[1,2],[1,2]],[[3,4],[3,4::Int]]]
