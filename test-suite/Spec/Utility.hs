{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Utility (module Spec.Utility) where

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

util ∷ Spec
util = parallel $ do

    it "mapBoth" $
        mapBoth show (1 :: Int, 2) `shouldBe` ("1", "2")

    it "mapTuple3" $
        mapTuple3 show (1 :: Int, 2, 3) `shouldBe` ("1", "2", "3")

    it "tuplify3" $
        tuplify3 [1 :: Int, 2, 3] `shouldBe` (1 :: Int, 2, 3)

    it "untuple3" $
        untuple3 (1 :: Int, 2, 3) `shouldBe` [1 :: Int, 2, 3]

    it "flatten" $
        flatten (Many [One [1], One [2]]) `shouldBe` [1 :: Int, 2]

    it "pick" $ do
        x <- pick [1 :: Int, 2, 3]
        x < 5 `shouldBe` True

    it "groupByVal" $
        groupByVal [(1 :: Int, "odd"), (2, "even"), (3, "odd")] `shouldBe` (insert "odd" [1, 3] (singleton "even" [2]) :: HashMap String [Int])

    it "fromKeys" $
        fromKeys show [1 :: Int, 2] `shouldBe` insert 1 "1" (singleton 2 "2")

    it "fromKeysM" $ do
        x <- fromKeysM (pure . show) [1 :: Int, 2]
        x `shouldBe` insert 1 "1" (singleton 2 "2")

    it "fromValsM" $ do
        x <- fromValsM (pure . show) [1 :: Int, 2]
        x `shouldBe` insert "1" 1 (singleton "2" 2)

    it "pickKeys" $ do
        let b = singleton "b" "B"
        pickKeys ["b"] (insert "a" "A" b) `shouldBe` b

    it "randomSplit" $ do
        GenerationConfig{..} <- parseGenerationConfig
        let stdGen :: StdGen = mkStdGen seed
        let (nums_train, _nums_validation, _nums_test) =
                randomSplit stdGen (0.5, 0.3, 0.2) [0 .. 9 :: Int]
        length nums_train `shouldBe` 5

    it "combineConfig" $ do
        synthCfg <- parseSynthesizerConfig
        GridSearchConfig{..} <- parseGridSearchConfig
        let optCfg = OptimizationConfig{..}
        let hparComb = HparComb
                { dropoutRate = 0.0
                , regularization = 0.0
                , m = 20
                , h = 30
                }
        combineConfig optCfg hparComb `shouldBe` synthCfg
