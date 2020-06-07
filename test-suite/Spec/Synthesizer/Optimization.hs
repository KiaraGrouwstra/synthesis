{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Synthesizer.Optimization (module Spec.Synthesizer.Optimization) where

import           Test.Tasty                   (TestTree, defaultMain, testGroup)
import           Test.HUnit.Base              (Test (..))
import           Test.HUnit.Text              (runTestTT)
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit             ((@?=))

import           Prelude                      hiding (abs, all)
import           Control.Exception            (SomeException, try, evaluate)
import           Control.Monad (mapM, join)
import           Data.Int                     (Int64)
import           Data.Maybe                   (isNothing)
import           Data.Either                  (fromRight, isRight)
import           Data.Functor                 (void, (<&>))
import           Data.Bifunctor               (first, second)
import           Data.HashMap.Lazy            (HashMap, empty, insert, singleton, (!), keys, fromList, size)
import qualified Data.Set as Set
import           Data.Yaml
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
import           Synthesis.Synthesizer.Train
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical  as Categorical
import           Synthesis.Synthesizer.Params
import           Synthesis.GridSearch
import           Spec.Synthesizer.Types

type Device = Cpu

optim ∷ Spec
optim = parallel $ do

    it "static parameter combinations" $ do
        EvolutionaryConfig{..} <- parseEvolutionaryConfig
        let cfg = OptimizationConfig{..}
        taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
        let TaskFnDataset{..} = taskFnDataset
        (length $ (flip (!!) $ head mOpts) $ getM @Device @FeatMult @0 @0 @0 @0 @0 @0 cfg taskFnDataset hparCombs) `shouldBe` (length hparCombs `div` length mOpts)
        (length . join        $ pickIdxs mOpts $ getM @Device @FeatMult @0 @0 @0 @0 @0 @0 cfg taskFnDataset hparCombs) `shouldBe` length hparCombs
        (length . join . join $ pickIdxs hOpts $ getH @Device @FeatMult @0 @0 @0 @0 @0    cfg taskFnDataset hparCombs) `shouldBe` length hparCombs * length mOpts
        -- putStrLn . show $ length hparCombs * length mOpts * length hOpts
        -- (length . join . join $ (!! longestString) $ getMaxStringLength @Device @FeatMult @0 @0 @0 @0 cfg taskFnDataset hparCombs) `shouldBe` length hparCombs * length mOpts * length hOpts
        -- (length . join . join $ (!! (size dsl + natValI @LhsSymbols)) $ getSymbols @Device @FeatMult @0 @0 @0 cfg taskFnDataset hparCombs) `shouldBe` length hparCombs * length mOpts * length hOpts
        -- (length . join . join $ (!! (size charMap + 1)) $ getMaxChar @Device @FeatMult @0 @0 cfg taskFnDataset hparCombs) `shouldBe` length hparCombs * length mOpts * length hOpts
        -- (length . join . join $ (!! length exprBlocks) $ getRules @Device @FeatMult @0 cfg taskFnDataset hparCombs) `shouldBe` length hparCombs * length mOpts * length hOpts
