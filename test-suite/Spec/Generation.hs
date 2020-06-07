{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Generation (module Spec.Generation) where

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

gen ∷ Test
gen = let
        bl = tyCon "Bool"
        int_ = tyCon "Int"
        tp = tyFun bl bl
        types_by_arity = insert 1 ["[]"] (singleton 0 ["Bool", "Int"])
    in TestList

    [ TestLabel "fnOutputs" $ TestCase $ do
        GenerationConfig { crashOnError = crashOnError } :: GenerationConfig <- parseGenerationConfig
        -- not
        hm1 <- interpretUnsafe $ fnOutputs crashOnError (singleton bl [con "True", con "False"]) (var "not") [([bl], bl)]
        pp_ hm1 `shouldBe` pp_ ((singleton (bl, bl) [(parseExpr "True", Right (parseExpr "False")), (parseExpr "False", Right (parseExpr "True"))]) :: HashMap (Tp, Tp) [(Expr, Either String Expr)])
        -- (+)
        hm2 <- interpretUnsafe $ fnOutputs crashOnError (singleton int_ (int <$> [1,2,3])) (parseExpr "(+)") [([int_,int_], int_)]
        pp_ hm2 `shouldBe` pp_ ((singleton (tyTuple [int_, int_], int_) [(parseExpr "(1,1)", Right (parseExpr "2")), (parseExpr "(1,2)", Right (parseExpr "3")), (parseExpr "(1,3)", Right (parseExpr "4")), (parseExpr "(2,1)", Right (parseExpr "3")), (parseExpr "(2,2)", Right (parseExpr "4")), (parseExpr "(2,3)", Right (parseExpr "5")), (parseExpr "(3,1)", Right (parseExpr "4")), (parseExpr "(3,2)", Right (parseExpr "5")), (parseExpr "(3,3)", Right (parseExpr "6"))]) :: HashMap (Tp, Tp) [(Expr, Either String Expr)])

    , TestLabel "fillHole" $ TestCase $ do
        let blockAsts' = singleton "not_" $ var "not"
        let expr = letIn blockAsts' $ expTypeSig holeExpr tp
        lst <- interpretUnsafe (fillHole blockAsts' Data.Set.empty [("not_", var "not_")] expr) <&> snd
        (gtrExpr . fstOf3 <$> lst) `shouldContain` [var "not_"]

    , TestLabel "fillHoles" $ TestCase $ do
        let blockAsts' = singleton "not_" $ var "not"
        let expr = expTypeSig holeExpr tp
        lst <- interpretUnsafe $ fillHoles 0 blockAsts' Data.Set.empty [("not_", var "not_")] expr
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    , TestLabel "genFns" $ TestCase $ do
        let blockAsts' = singleton "not_" $ var "not"
        lst <- interpretUnsafe $ genFns 0 [("not_", var "not_")] blockAsts'
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    , TestLabel "instantiateTypes" $ TestCase $ do
        let lst_ = tyCon "[]"
        -- a => Set a
        let set_ s = tyApp (tyCon "Set") $ tyCon s
        l1 <- interpretUnsafe $ instantiateTypes types_by_arity (singleton 0 [bl, int_]) (tyApp (tyCon "Set") $ tyVar "b")
        (pp <$> l1) `shouldContain` (pp <$> [set_ "Bool", set_ "Int"])
        -- Num a => a -> a -> a
        let a = tyVar "a"
        l2 <- interpretUnsafe $ instantiateTypes types_by_arity (singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA "Num" a]) $ tyFun a $ tyFun a a)
        (pp <$> l2) `shouldBe` (pp <$> [tyFun int_ $ tyFun int_ int_])
        -- Ord a => [a] -> [a]
        l3 <- interpretUnsafe $ instantiateTypes types_by_arity (singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA "Ord" a]) $ tyFun (tyList a) $ tyList a)
        (pp <$> l3) `shouldBe` (pp <$> [tyFun (tyList bl) $ tyList bl, tyFun (tyList int_) $ tyList int_])
        -- Foldable t => t Bool -> Bool
        l4 <- interpretUnsafe $ instantiateTypes types_by_arity (insert 1 [lst_] $ singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA "Foldable" a]) $ tyFun (tyApp a bl) bl)
        (pp <$> l4) `shouldBe` (pp <$> [tyFun (tyApp lst_ bl) bl])
        -- Foldable t => t a -> Bool
        let t = tyVar "t"
        l5 <- interpretUnsafe $ instantiateTypes types_by_arity (insert 1 [lst_] $ singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA "Foldable" t]) $ tyFun (tyApp t a) bl)
        (pp <$> l5) `shouldBe` (pp <$> [tyFun (tyApp lst_ bl) bl, tyFun (tyApp lst_ int_) bl])

    , TestLabel "instantiateTypeVars" $ TestCase $ do
        let lst_ = tyCon "[]"
        -- without type constraint
        l1 <- interpretUnsafe $ instantiateTypeVars types_by_arity (singleton 0 [bl, int_]) $ singleton "a" (0, [])
        l1 `shouldBe` [singleton "a" bl, singleton "a" int_]
        -- with type constraint
        l2 <- interpretUnsafe $ instantiateTypeVars types_by_arity (singleton 0 [bl, int_]) $ singleton "a" (0, [tyCon "Num"])
        l2 `shouldBe` [singleton "a" int_]
        -- Ord a => [a] -> [a]
        l3 <- interpretUnsafe $ instantiateTypeVars types_by_arity (singleton 0 [bl, int_]) $ singleton "a" (0, [tyCon "Ord"])
        l3 `shouldBe` [singleton "a" bl, singleton "a" int_]
        -- Foldable t => t Bool -> Bool
        l4 <- interpretUnsafe $ instantiateTypeVars types_by_arity (insert 1 [lst_] $ singleton 0 [bl, int_]) $ singleton "t" (1, [tyCon "Foldable"])
        l4 `shouldBe` [singleton "t" lst_]
        -- Foldable t => t a -> Bool
        l5 <- interpretUnsafe $ instantiateTypeVars types_by_arity (insert 1 [lst_] $ singleton 0 [bl, int_]) $ insert "a" (0, []) $ singleton "t" (1, [tyCon "Foldable"])
        pp_ l5 `shouldBe` pp_ [insert "a" bl (singleton "t" lst_), insert "a" int_ (singleton "t" lst_)]

    , TestLabel "matchesType" $ TestCase $ do
        let a = tyVar "a"
        q <- interpretUnsafe $ matchesType int_ a
        q `shouldBe` True
        w <- interpretUnsafe $ matchesType a bl
        w `shouldBe` True
        e <- interpretUnsafe $ matchesType bl bl
        e `shouldBe` True
        r <- interpretUnsafe $ matchesType bl int_
        r `shouldBe` False
        t <- interpretUnsafe $ matchesType a a
        t `shouldBe` True
        y <- interpretUnsafe $ matchesType a $ tyVar "b"
        y `shouldBe` True

    , TestLabel "matchesConstraints" $ TestCase $ do
        -- Bool <-> Foldable
        y <- interpretUnsafe $ matchesConstraints 0 bl [tyCon "Foldable"]
        y `shouldBe` False
        -- Int <-> Enum
        x <- interpretUnsafe $ matchesConstraints 0 int_ [tyCon "Enum"]
        x `shouldBe` True
        -- [] <-> Foldable
        let lst = tyCon "[]"
        z <- interpretUnsafe $ matchesConstraints 1 lst [tyCon "Foldable"]
        z `shouldBe` True

    ]
