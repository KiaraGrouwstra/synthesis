-- Tasty: <http://documentup.com/feuerbach/tasty>
import Test.Tasty (TestTree, defaultMain, testGroup)
-- Hspec: <https://hspec.github.io>
import Test.Tasty.Hspec
import Test.Tasty.HUnit (testCase, (@?=))
import Test.HUnit.Base (Test(..))
import Test.HUnit.Text (runTestTT)

import Data.HashMap.Lazy (HashMap, empty, insert, singleton)
import Control.Lens
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.Syntax ( Exp(..), SpecialCon(..), Type(..), Name(..), QName(..), Type(..) )
import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), runInterpreter, interpret, as, lift, liftIO, runStmt)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.Functor (void)

import Utility
import Hint
import Types
import FindHoles
import Ast
import Config

main :: IO ()
main = do
    -- HUnit is illegible but helps ensure the Interpreter is run only once...
    void $ runTestTT $ TestList [hint, ast]

    -- Tasty HSpec
    utilSpec <- testSpec "Utility" util
    typesSpec <- testSpec "Types" types
    findSpec <- testSpec "FindHoles" find
    let tree = testGroup "synthesis" [utilSpec, typesSpec, findSpec]
    defaultMain tree

util :: Spec
util = parallel $ do

    it "mapTuple" $
        mapTuple show (1, 2) `shouldBe` ("1", "2")

    it "flatten" $
        flatten (Many [One [1], One [2]]) `shouldBe` [1, 2]

    it "flattenTuple" $
        flattenTuple (DeepTuple (1, SingleTuple (2, 3))) `shouldBe` [1, 2, 3]

    it "pick" $ do
        x <- pick [1, 2, 3]
        x < 5 `shouldBe` True

    it "groupByVal" $
        groupByVal [(1 :: Int, "odd"), (2, "even"), (3, "odd")] `shouldBe` (insert "odd" [1, 3] (singleton "even" [2]) :: HashMap String [Int])

    it "toMapBy" $
        toMapBy [1 :: Int, 2] show `shouldBe` insert 1 "1" (singleton 2 "2")

hint :: Test
hint = TestList

    [ TestLabel "runInterpreter_" $ TestCase $ do
        x <- runInterpreter_ $ runStmt "\"foo\""
        x @?= ()

    , TestLabel "say" $ TestCase $ do
        x <- runInterpreter $ say "hi"
        fromRight () x @?= ()

    , TestLabel "errorString" $ TestCase $ do
        r <- runInterpreter $ interpret "foo" (as :: String)
        let s = case r of
                Left err -> errorString err
                _ -> ""
        not (null s) @?= True

    , TestLabel "interpretIO" $ TestCase $ do
        x <- runInterpreter (interpretIO "return \"foo\"")
        fromRight "" x @?= "foo"

    , TestLabel "fnIoPairs" $ TestCase $ do
        x <- runInterpreter (fnIoPairs "not" "[True, False]")
        fromRight "" x @?= "[(True, False), (False, True)]"

    , TestLabel "fnIoPairs" $ TestCase $ do
        x <- runInterpreter (genInputs 10 "Bool")
        fromRight "" x `shouldContain` "True"
    ]

types :: Spec
types = parallel $ do

    let bl = tyCon "Bool"
    let int = tyCon "Int"

    it "varNode" $
        prettyPrint (varNode "foo") @?= "foo"

    it "tyVar" $
        prettyPrint (tyVar "a") `shouldBe` "a"

    it "tyCon" $
        prettyPrint (tyCon "Int") `shouldBe` "Int"

    it "tyApp" $
        prettyPrint (tyApp "[]" $ tyCon "Int") `shouldBe` "[] Int"

    it "fnTypeIO" $ do
        let a = tyVar "a"
        let b = tyVar "b"
        fnTypeIO (TyFun l a b) `shouldBe` (a, b)

    it "findTypeVars" $
        findTypeVars (TyFun l (tyVar "a") (TyApp l (tyCon "Set") (tyVar "b"))) `shouldBe` ["a", "b"]

    it "randomType" $ do
        tp <- randomType False False nestLimit [] 0
        [tyCon "Bool", tyCon "Int"] `shouldContain` [tp]

    it "randomFnType" $ do
        tp <- randomFnType False False nestLimit [] 0
        [TyFun l bl bl, TyFun l bl int, TyFun l int bl, TyFun l int int] `shouldContain` [tp]

    it "genTypes" $ do
        tps <- nub . flatten <$> genTypes 0 10
        tps `shouldContain` [bl, int]

    it "instantiateTypes" $ do
        let set s = TyApp l (tyCon "Set") (tyCon s)
        instantiateTypes [bl, int] (TyApp l (tyCon "Set") (tyVar "b")) `shouldContain` [set "Bool", set "Int"]

    it "instantiateTypeVars" $
        instantiateTypeVars [bl, int] ["a"] `shouldBe` [singleton "a" bl, singleton "a" int]

    it "fillTypeVars" $
        fillTypeVars (TyFun l int (tyVar "a")) (singleton "a" bl) `shouldBe` TyFun l int bl

find :: Spec
find = do

    it "findHolesExpr" $ do
        let expr = fromParseResult (parse "(_ :: Int -> Bool)" :: ParseResult Expr)
        let hole_lenses = findHolesExpr expr
        let hole_lens = head hole_lenses
        let hole_getter :: (Expr -> Const Expr Expr) -> Expr -> Const Expr Expr = fst hole_lens
        let hole_setter :: (Expr -> Identity Expr) -> Expr -> Identity Expr = snd hole_lens
        let hole :: Expr = view hole_getter expr
        prettyPrint hole `shouldBe` "_ :: Int -> Bool"
        let xpr = set hole_setter expr holeExpr
        prettyPrint xpr `shouldBe` "(_)"

ast :: Test
ast = let 
        bl = tyCon "Bool"
        int = tyCon "Int"
    in TestList

    [ TestLabel "skeleton" $ TestCase $
        prettyPrint (skeleton bl) `shouldBe` "_ :: Bool"

    , TestLabel "numAstNodes" $ TestCase $
        numAstNodes holeExpr `shouldBe` 3

    , TestLabel "filterTypeSigIoFns" $ TestCase $ do
        let fn_asts = insert "not" (varNode "not") $ singleton "not_" $ varNode "not"
        map <- runInterpreter $ filterTypeSigIoFns fn_asts (singleton "Bool -> Bool" $ singleton "[(True, False), (False, True)]" ["not", "not_"])
        fromRight empty map `shouldBe` singleton "Bool -> Bool" (singleton "[(True, False), (False, True)]" "not")

    , TestLabel "fnOutputs" $ TestCase $ do
        io <- runInterpreter $ fnOutputs (singleton "not" "not") (singleton "Bool -> Bool" "[True, False]") "not" ["Bool -> Bool"]
        let hm = case io of
                    Right map -> map
                    Left _ -> empty
        hm `shouldBe` singleton "Bool -> Bool" "[(True, False), (False, True)]"

    , TestLabel "fillHole" $ TestCase $ do
        let tp = TyFun l bl bl
        filled <- runInterpreter $ fillHole 0 (singleton "not" tp) (ExpTypeSig l holeExpr tp)
        let lst = case filled of
                    Right l -> l
                    Left _ -> []
        lst `shouldContain` [varNode "not"]
    ]
