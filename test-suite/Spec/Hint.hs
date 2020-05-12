{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Hint (module Spec.Hint) where

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

hint ∷ Test
hint = let
        bl = tyCon "Bool"
        int_ = tyCon "Int"
        infTp :: String = "div (const const) div"
    in TestList

    [ TestLabel "interpretSafe" $ TestCase $ do
        x <- interpretSafe $ interpret "\"foo\"" (as :: String)
        fromRight "" x @?= "foo"

    , TestLabel "say" $ TestCase $ do
        x <- interpretUnsafe $ return ()
        x @?= ()

    , TestLabel "errorString" $ TestCase $ do
        s <- interpretSafe (interpret "foo" (as :: String)) <&> \case
            Left err_ -> errorString err_
            _ -> ""
        not (null s) @?= True

    , TestLabel "interpretIO" $ TestCase $ do
        GenerationConfig { crashOnError = crashOnError } :: GenerationConfig <- parseGenerationConfig
        x <- interpretUnsafe (fromRight "" <$> interpretIO crashOnError "return \"foo\"")
        x @?= "foo"

    , TestLabel "fnIoPairs" $ TestCase $ do
        let crashOnError = False
        -- n=1
        x <- interpretUnsafe $ fnIoPairs crashOnError 1 (var "not") (tplIfMultiple [bl]) $ parseExpr "[True, False]"
        pp_ x @?= pp_ ([(parseExpr "True", Right (parseExpr "False")), (parseExpr "False", Right (parseExpr "True"))] :: [(Expr, Either String Expr)])
        -- n=2
        q <- interpretUnsafe $ fnIoPairs crashOnError 2 (parseExpr "(+)") (tplIfMultiple [int_,int_]) $ parseExpr "[(1,2),(3,4)]"
        pp_ q @?= pp_ ([(parseExpr "(1, 2)", Right (parseExpr "3")), (parseExpr "(3, 4)", Right (parseExpr "7"))] :: [(Expr, Either String Expr)])
        -- run-time error
        x <- interpretUnsafe $ fnIoPairs crashOnError 1 (var "succ") (tplIfMultiple [bl]) $ parseExpr "[False, True]"
        pp_ x @?= pp_ ([(parseExpr "False", Right (parseExpr "True")), (parseExpr "True", Left "\"Prelude.Enum.Bool.succ: bad argument\"")] :: [(Expr, Either String Expr)])
        -- bad type
        errored <- isNothing <.> timeout 10000 . interpretUnsafe . fnIoPairs crashOnError 1 (parseExpr "div (const const) div") (tplIfMultiple [bl]) $ list []
        errored `shouldBe` True

    , TestLabel "exprType" $ TestCase $ do
        x <- interpretUnsafe $ exprType $ parseExpr "True"
        pp x `shouldBe` "Bool"
        errored <- isNothing <.> timeout 10000 . interpretUnsafe . exprType $ parseExpr "div (const const) div"
        errored `shouldBe` True

    , TestLabel "handling infinite types: typeChecks" $ TestCase $ do

        -- typeChecks
        errored <- not <.> interpretUnsafe . typeChecks $ infTp
        errored `shouldBe` True

        -- typeChecksWithDetails
        either <- interpretUnsafe . typeChecksWithDetails $ infTp
        let errored = not $ isRight either
        errored `shouldBe` True

    , TestLabel "handling infinite types: typeOf" $ TestCase $ do

        -- type check + typeOf
        let timeout_micros :: Int = 100000
        either <- interpretUnsafe . typeChecksWithDetails $ infTp
        errored <- case either of
            Right _ -> isNothing <.> timeout timeout_micros . interpretUnsafe . exprType . parseExpr $ infTp
            _ -> pure True
        errored `shouldBe` True

    , TestLabel "handling infinite types: fnIoPairs" $ TestCase $ do

        -- fnIoPairs + type check
        let n = 1
        let crash_on_error = False
        errored <- null <.> interpretUnsafe . fnIoPairs crash_on_error n (parseExpr infTp) (tplIfMultiple [bl]) $ list []
        errored `shouldBe` True

    ]
