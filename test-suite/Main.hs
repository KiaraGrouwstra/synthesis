{-# LANGUAGE PackageImports, LambdaCase #-}

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
import qualified Data.Set
import Util (fstOf3)

import Utility
import Hint
import Types
import FindHoles
import Ast
import Generation
import Configs

main :: IO ()
main = do
    -- unlike Tasy, HUnit's default printer is illegible,
    -- but helps ensure the Interpreter is run only once...
    void $ runTestTT $ TestList [hint, gen]

    -- Tasty HSpec
    util_ <- testSpec "Utility" util
    types_ <- testSpec "Types" types
    find_ <- testSpec "FindHoles" find
    ast_ <- testSpec "Ast" ast
    let tree :: TestTree = testGroup "synthesis" [util_, types_, find_, ast_]
    defaultMain tree

util :: Spec
util = parallel $ do

    it "mapTuple" $
        mapTuple show (1 :: Int, 2) `shouldBe` ("1", "2")

    it "mapTuple3" $
        mapTuple3 show (1 :: Int, 2, 3) `shouldBe` ("1", "2", "3")

    it "tuplify3" $
        tuplify3 [1 :: Int, 2, 3] `shouldBe` (1 :: Int, 2, 3)

    it "untuple3" $
        untuple3 (1 :: Int, 2, 3) `shouldBe` [1 :: Int, 2, 3]

    it "flatten" $
        flatten (Many [One [1], One [2]]) `shouldBe` [1 :: Int, 2]

    it "flattenTuple" $
        flattenTuple (DeepTuple (1, SingleTuple (2, 3))) `shouldBe` [1 :: Int, 2, 3]

    it "pick" $ do
        x <- pick [1 :: Int, 2, 3]
        x < 5 `shouldBe` True

    it "groupByVal" $
        groupByVal [(1 :: Int, "odd"), (2, "even"), (3, "odd")] `shouldBe` (insert "odd" [1, 3] (singleton "even" [2]) :: HashMap String [Int])

    it "fromKeys" $
        fromKeys show [1 :: Int, 2] `shouldBe` insert 1 "1" (singleton 2 "2")

    it "fromVals" $
        fromVals show [1 :: Int, 2] `shouldBe` insert "1" 1 (singleton "2" 2)

    it "pickKeys" $ do
        let b = singleton "b" "B"
        pickKeys ["b"] (insert "a" "A" b) `shouldBe` b

    -- it "while" $
    --     while ((< 5) . length) (\ls -> ((([head ls] :: String) ++ (ls :: String))) :: String) "ah" `shouldBe` "aaaah"

    it "randomSplit" $ do
        let (train, validation, test) = randomSplit (0.5, 0.3, 0.2) [0 .. 9 :: Int]
        length train `shouldBe` 5

hint :: Test
hint = let
        bl = tyCon "Bool"
    in TestList

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
        fromRight "" x @?= "[(True,Right False),(False,Right True)]"

    , TestLabel "genInputs" $ TestCase $ do
        x <- runInterpreter_ (genInputs 10 "Bool")
        fromRight "" x `shouldContain` "True"

    , TestLabel "exprType" $ TestCase $ do
        x <- runInterpreter_ (exprType $ parseExpr "True")
        pp (fromRight undefined x) `shouldBe` "Bool"

    ]

types :: Spec
types = parallel $ do

    let bl = tyCon "Bool"
    let int = tyCon "Int"
    let str = tyCon "String"

    it "var" $
        pp (var "foo") @?= "foo"

    it "tyVar" $
        pp (tyVar "a") `shouldBe` "a"

    it "tyCon" $
        pp (tyCon "Int") `shouldBe` "Int"

    it "tyApp" $
        pp (tyApp (tyCon "[]") $ tyCon "Int") `shouldBe` "[] Int"

    it "expTypeSig" $
        -- pp (expTypeSig holeExpr $ tyCon "Int") `shouldBe` "_ :: Int"
        pp (expTypeSig holeExpr $ tyCon "Int") `shouldBe` "undefined :: Int"

    it "fnTypeIO" $ do
        let a = tyVar "a"
        let b = tyVar "b"
        fnTypeIO (tyFun a b) `shouldBe` (a, b)

    it "findTypeVars" $
        findTypeVars (tyFun (tyVar "a") (TyApp l (tyCon "Set") (tyVar "b"))) `shouldBe` insert "a" [] (singleton "b" [])

    it "randomType" $ do
        tp <- randomType False False nestLimit empty 0
        [tyCon "Bool", tyCon "Int"] `shouldContain` [tp]

    it "randomFnType" $ do
        tp <- randomFnType False False nestLimit empty 0
        [tyFun bl bl, tyFun bl int, tyFun int bl, tyFun int int] `shouldContain` [tp]

    it "genTypes" $ do
        tps <- nub . flatten <$> genTypes 0 10
        tps `shouldContain` [bl]

    it "fillTypeVars" $
        fillTypeVars (tyFun int (tyVar "a")) (singleton "a" bl) `shouldBe` tyFun int bl
    
    it "mergeTyVars" $
        mergeTyVars (singleton "a" [bl, str]) (singleton "a" [int, bl]) `shouldBe` singleton "a" [bl, str, int]
    
    it "parseExpr" $
        pp (parseExpr "a") `shouldBe` "a"

    it "parseType" $
        pp (parseType "a") `shouldBe` "a"

find :: Spec
find = -- do

    it "findHolesExpr" $ do
        -- let expr = parseExpr "(_ :: Int -> Bool)"
        let expr = parseExpr "(undefined :: Int -> Bool)"
        let hole_lenses = findHolesExpr expr
        -- print $ show hole_lenses
        let hole_lens = head hole_lenses
        let hole_getter = fst hole_lens
        let hole_setter = snd hole_lens
        let hole :: Expr = hole_getter expr
        -- pp hole `shouldBe` "_ :: Int -> Bool"
        pp hole `shouldBe` "undefined :: Int -> Bool"
        let xpr = hole_setter expr holeExpr
        -- pp xpr `shouldBe` "(_)"
        pp xpr `shouldBe` "(undefined)"

ast :: Spec
ast = do

    it "skeleton" $ do
        let bl = tyCon "Bool"
        -- pp (skeleton bl) `shouldBe` "_ :: Bool"
        pp (skeleton bl) `shouldBe` "undefined :: Bool"

    it "numAstNodes" $
        numAstNodes holeExpr `shouldBe` 3

    it "hasHoles" $ do
        hasHoles holeExpr `shouldBe` False
        let expr = expTypeSig holeExpr $ tyCon "Int"
        hasHoles expr `shouldBe` True

    it "filterTypeSigIoFns" $ do
        let fn_asts = insert "not" (var "not") $ singleton "not_" $ var "not"
        let hm = filterTypeSigIoFns fn_asts (singleton "Bool -> Bool" $ singleton "[(True, False), (False, True)]" ["not", "not_"])
        hm `shouldBe` singleton "Bool -> Bool" (singleton "[(True, False), (False, True)]" "not")

gen :: Test
gen = let 
        bl = tyCon "Bool"
        int = tyCon "Int"
        tp = tyFun bl bl
        maybeList = fmap $ \case
                    Right ls -> ls
                    Left _ -> []
    in TestList

    [ TestLabel "fnOutputs" $ TestCase $ do
        io <- runInterpreter_ $ fnOutputs (singleton "Bool" "[True, False]") "not" ["Bool"]
        let hm = case io of
                    Right m -> m
                    Left _ -> empty
        hm `shouldBe` singleton "Bool" "[(True,Right False),(False,Right True)]"

    , TestLabel "fillHole" $ TestCase $ do
        let blockAsts = singleton "not_" $ var "not"
        let expr = letIn blockAsts $ expTypeSig holeExpr tp
        filled <- runInterpreter_ $ fillHole blockAsts Data.Set.empty [("not_", var "not_")] expr
        -- print $ case filled of
        --         Right _ -> ""
        --         Left e -> errorString e
        let lst = case filled of
                    Right (_partial, candidates) -> candidates
                    Left _ -> []
        (gtrExpr . fstOf3 <$> lst) `shouldContain` [var "not_"]

    , TestLabel "fillHoles" $ TestCase $ do
        let blockAsts = singleton "not_" $ var "not"
        let expr = expTypeSig holeExpr tp
        lst <- maybeList . runInterpreter_ $ fillHoles 0 blockAsts Data.Set.empty [("not_", var "not_")] expr
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    , TestLabel "genFn" $ TestCase $ do
        let blockAsts = singleton "not_" $ var "not"
        filled <- runInterpreter_ $ genFn 0 [("not_", var "not_")] blockAsts
        isRight filled `shouldBe` True

    , TestLabel "genFns" $ TestCase $ do
        let blockAsts = singleton "not_" $ var "not"
        lst <- maybeList . runInterpreter_ $ genFns 0 [("not_", var "not_")] blockAsts
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    , TestLabel "instantiateTypes" $ TestCase $ do
        let set_ s = TyApp l (tyCon "Set") (tyCon s)
        lst <- maybeList . runInterpreter_ $ instantiateTypes [bl, int] (TyApp l (tyCon "Set") (tyVar "b"))
        lst `shouldContain` [set_ "Bool", set_ "Int"]

    , TestLabel "instantiateTypeVars" $ TestCase $ do
        -- without type constraint
        lst <- maybeList . runInterpreter_ $ instantiateTypeVars [bl, int] $ singleton "a" []
        lst `shouldBe` [singleton "a" bl, singleton "a" int]
        -- with type constraint
        lst <- maybeList . runInterpreter_ $ instantiateTypeVars [bl, int] $ singleton "a" [tyCon "Num"]
        lst `shouldBe` [singleton "a" int]

    , TestLabel "matchesConstraints" $ TestCase $ do
        x <- runInterpreter_ $ matchesConstraints int [tyCon "Enum"]
        fromRight False x `shouldBe` True

    ]
