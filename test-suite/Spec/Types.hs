{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Types (module Spec.Types) where

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

types ∷ Spec
types = parallel $ let
        bl = tyCon "Bool"
        int_ = tyCon "Int"
    in do

    it "var" $
        pp (var "foo") @?= "foo"

    it "tyVar" $
        pp (tyVar "a") `shouldBe` "a"

    it "tyCon" $
        pp (tyCon "Int") `shouldBe` "Int"

    it "tyApp" $
        pp (tyApp (tyCon "[]") $ tyCon "Int") `shouldBe` "[] Int"

    it "expTypeSig" $
        pp (expTypeSig holeExpr $ tyCon "Int") `shouldBe` "undefined :: Int"

    it "fnTypeIO" $ do
        let a = tyVar "a"
        let b = tyVar "b"
        fnTypeIO (tyFun a b) `shouldBe` ([a], b)

    it "parseExpr" $
        pp (parseExpr "a") `shouldBe` "a"

    it "parseType" $ do
        pp (parseType "a") `shouldBe` "a"
        let s = "  (Eq (a -> Bool)) => a"
        pp (parseType s) `shouldBe` s

    it "isFn" $ do
        isFn bl `shouldBe` False
        isFn (tyVar "a") `shouldBe` False
        isFn (tyFun bl int_) `shouldBe` True
    
    it "typeSane" $ do
        typeSane bl `shouldBe` True
        typeSane (tyList bl) `shouldBe` True
        typeSane (tyFun bl bl) `shouldBe` True
        typeSane (tyFun (tyFun bl bl) (tyFun bl bl)) `shouldBe` True
        typeSane (tyList (tyFun bl bl)) `shouldBe` False
        typeSane (tyFun (tyList (tyFun bl bl)) (tyFun bl bl)) `shouldBe` False
        typeSane (tyFun (tyFun bl bl) (tyList (tyFun bl bl))) `shouldBe` False
        typeSane (tyParen (tyFun bl bl)) `shouldBe` True
        let a = tyVar "a"
        -- (Eq (a -> Bool)) => a
        typeSane (tyForall Nothing (Just (cxTuple [typeA "Eq" (tyFun a (tyCon "Bool"))])) a) `shouldBe` False
        -- I guess this means I'd judge HaskTorch's Typed functions as insane, but
        -- for the purpose of program synthesis, for the moment let's say they are.
