-- Tasty: <http://documentup.com/feuerbach/tasty>
import Test.Tasty (TestTree, defaultMain, testGroup)
-- Hspec: <https://hspec.github.io>
import Test.Tasty.Hspec
import Test.Tasty.HUnit ((@?=))
import Test.HUnit.Base (Test(..))
import Test.HUnit.Text (runTestTT)

import Data.HashMap.Lazy (HashMap, empty, insert, singleton)
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.Syntax ( Type(..) )
import Language.Haskell.Interpreter (interpret, as)
import Data.List (nub)
import Data.Either (fromRight, isRight)
import Data.Functor (void)

import Utility
import Hint
import Types
import FindHoles
import Ast
import Config

main :: IO ()
main = do
    -- unlike Tasy, HUnit's default printer is illegible,
    -- but helps ensure the Interpreter is run only once...
    void $ runTestTT $ TestList [hint, ast]

    -- Tasty HSpec
    util_ <- testSpec "Utility" util
    types_ <- testSpec "Types" types
    find_ <- testSpec "FindHoles" find
    let tree :: TestTree = testGroup "synthesis" [util_, types_, find_]
    defaultMain tree

util :: Spec
util = parallel $ do

    it "mapTuple" $
        mapTuple show (1 :: Int, 2) `shouldBe` ("1", "2")

    it "flatten" $
        flatten (Many [One [1], One [2]]) `shouldBe` [1 :: Int, 2]

    it "flattenTuple" $
        flattenTuple (DeepTuple (1, SingleTuple (2, 3))) `shouldBe` [1 :: Int, 2, 3]

    it "pick" $ do
        x <- pick [1 :: Int, 2, 3]
        x < 5 `shouldBe` True

    it "groupByVal" $
        groupByVal [(1 :: Int, "odd"), (2, "even"), (3, "odd")] `shouldBe` (insert "odd" [1, 3] (singleton "even" [2]) :: HashMap String [Int])

    it "toMapBy" $
        toMapBy [1 :: Int, 2] show `shouldBe` insert 1 "1" (singleton 2 "2")

    it "pickKeys" $ do
        let b = singleton "b" "B"
        pickKeys ["b"] (insert "a" "A" b) `shouldBe` b

    -- it "while" $
    --     while ((< 5) . length) (\ls -> ((([head ls] :: String) ++ (ls :: String))) :: String) "ah" `shouldBe` "aaaah"

hint :: Test
hint = TestList

    [ TestLabel "runInterpreter_" $ TestCase $ do
        x <- runInterpreter_ $ interpret "\"foo\"" (as :: String)
        fromRight "" x @?= "foo"

    , TestLabel "say" $ TestCase $ do
        x <- runInterpreter_ $ say "hi"
        fromRight () x @?= ()

    , TestLabel "errorString" $ TestCase $ do
        r <- runInterpreter_ $ interpret "foo" (as :: String)
        let s = case r of
                Left err -> errorString err
                _ -> ""
        not (null s) @?= True

    , TestLabel "interpretIO" $ TestCase $ do
        x <- runInterpreter_ (interpretIO "return \"foo\"")
        fromRight "" x @?= "foo"

    , TestLabel "fnIoPairs" $ TestCase $ do
        x <- runInterpreter_ (fnIoPairs "not" "[True, False]")
        fromRight "" x @?= "[(True,False),(False,True)]"

    , TestLabel "genInputs" $ TestCase $ do
        x <- runInterpreter_ (genInputs 10 "Bool")
        fromRight "" x `shouldContain` "True"
    ]

types :: Spec
types = parallel $ do

    let bl = tyCon "Bool"
    let int = tyCon "Int"

    it "var" $
        pp (var "foo") @?= "foo"

    it "tyVar" $
        pp (tyVar "a") `shouldBe` "a"

    it "tyCon" $
        pp (tyCon "Int") `shouldBe` "Int"

    it "tyApp" $
        pp (tyApp (tyCon "[]") $ tyCon "Int") `shouldBe` "[] Int"

    it "expTypeSig" $
        pp (expTypeSig holeExpr $ tyCon "Int") `shouldBe` "_ :: Int"

    it "fnTypeIO" $ do
        let a = tyVar "a"
        let b = tyVar "b"
        fnTypeIO (tyFun a b) `shouldBe` (a, b)

    it "findTypeVars" $
        findTypeVars (tyFun (tyVar "a") (TyApp l (tyCon "Set") (tyVar "b"))) `shouldBe` ["a", "b"]

    it "randomType" $ do
        tp <- randomType False False nestLimit [] 0
        [tyCon "Bool", tyCon "Int"] `shouldContain` [tp]

    it "randomFnType" $ do
        tp <- randomFnType False False nestLimit [] 0
        [tyFun bl bl, tyFun bl int, tyFun int bl, tyFun int int] `shouldContain` [tp]

    it "genTypes" $ do
        tps <- nub . flatten <$> genTypes 0 10
        tps `shouldContain` [bl]

    it "instantiateTypes" $ do
        let set_ s = TyApp l (tyCon "Set") (tyCon s)
        instantiateTypes [bl, int] (TyApp l (tyCon "Set") (tyVar "b")) `shouldContain` [set_ "Bool", set_ "Int"]

    it "instantiateTypeVars" $
        instantiateTypeVars [bl, int] ["a"] `shouldBe` [singleton "a" bl, singleton "a" int]

    it "fillTypeVars" $
        fillTypeVars (tyFun int (tyVar "a")) (singleton "a" bl) `shouldBe` tyFun int bl

find :: Spec
find = -- do

    it "findHolesExpr" $ do
        let expr = fromParseResult (parse "(_ :: Int -> Bool)" :: ParseResult Expr)
        let hole_lenses = findHolesExpr expr
        -- print $ show hole_lenses
        let hole_lens = head hole_lenses
        let hole_getter = fst hole_lens
        let hole_setter = snd hole_lens
        let hole :: Expr = hole_getter expr
        pp hole `shouldBe` "_ :: Int -> Bool"
        let xpr = hole_setter expr holeExpr
        pp xpr `shouldBe` "(_)"

ast :: Test
ast = let 
        bl = tyCon "Bool"
        -- int = tyCon "Int"
    in TestList

    [ TestLabel "skeleton" $ TestCase $
        pp (skeleton bl) `shouldBe` "_ :: Bool"

    , TestLabel "numAstNodes" $ TestCase $
        numAstNodes holeExpr `shouldBe` 3

    , TestLabel "filterTypeSigIoFns" $ TestCase $ do
        let fn_asts = insert "not" (var "not") $ singleton "not_" $ var "not"
        hm <- runInterpreter_ $ filterTypeSigIoFns fn_asts (singleton "Bool -> Bool" $ singleton "[(True, False), (False, True)]" ["not", "not_"])
        fromRight empty hm `shouldBe` singleton "Bool -> Bool" (singleton "[(True, False), (False, True)]" "not")

    , TestLabel "fnOutputs" $ TestCase $ do
        io <- runInterpreter_ $ fnOutputs (singleton "not" "not") (singleton "Bool -> Bool" "[True, False]") "not" ["Bool -> Bool"]
        let hm = case io of
                    Right m -> m
                    Left _ -> empty
        hm `shouldBe` singleton "Bool -> Bool" "[(True,False),(False,True)]"

    , TestLabel "hasHoles" $ TestCase $ do
        hasHoles holeExpr `shouldBe` False
        hasHoles (expTypeSig holeExpr $ tyCon "Int") `shouldBe` True

    , TestLabel "fillHole" $ TestCase $ do
        let tp = tyFun bl bl
        let block_asts = singleton "not_" $ var "not"
        let expr = letIn block_asts $ expTypeSig holeExpr tp
        filled <- runInterpreter_ $ fillHole 0 (singleton "not_" tp) expr
        -- print $ case filled of
        --         Right _ -> ""
        --         Left e -> errorString e
        let lst = case filled of
                    Right (_partial, candidates) -> candidates
                    Left _ -> []
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    , TestLabel "fillHoles" $ TestCase $ do
        let tp = tyFun bl bl
        let block_asts = singleton "not_" $ var "not"
        let expr = letIn block_asts $ expTypeSig holeExpr tp
        filled <- runInterpreter_ $ fillHoles 0 (singleton "not_" tp) expr
        let lst = case filled of
                    Right ls -> ls
                    Left _ -> []
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    , TestLabel "genFn" $ TestCase $ do
        let tp = tyFun bl bl
        let block_asts = singleton "not_" $ var "not"
        filled <- runInterpreter_ $ genFn 0 (singleton "not_" tp) block_asts
        isRight filled `shouldBe` True

    , TestLabel "genFns" $ TestCase $ do
        let tp = tyFun bl bl
        let block_asts = singleton "not_" $ var "not"
        filled <- runInterpreter_ $ genFns 0 (singleton "not_" tp) block_asts
        let lst = case filled of
                    Right ls -> ls
                    Left _ -> []
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    ]
