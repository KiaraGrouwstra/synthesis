{-# LANGUAGE PackageImports, LambdaCase #-}

-- Tasty: <http://documentup.com/feuerbach/tasty>
import Test.Tasty (TestTree, defaultMain, testGroup)
-- Hspec: <https://hspec.github.io>
import Test.Tasty.Hspec
import Test.Tasty.HUnit ((@?=))
import Test.HUnit.Base (Test(..))
import Test.HUnit.Text (runTestTT)

import Data.HashMap.Lazy (HashMap, empty, insert, singleton)
import Language.Haskell.Interpreter (interpret, as)
import Data.List (nub)
import Data.Either (fromRight, isRight)
import Data.Functor (void)
import qualified Data.Set
import Util (fstOf3)

import Synthesis.Utility
import Synthesis.Hint
import Synthesis.Types
import Synthesis.FindHoles
import Synthesis.Ast
import Synthesis.Generation
import Synthesis.Configs

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

    it "fromKeysM" $ do
        x <- fromKeysM (pure . show) [1 :: Int, 2]
        x `shouldBe` insert 1 "1" (singleton 2 "2")

    it "fromValsM" $ do
        x <- fromValsM (pure . show) [1 :: Int, 2]
        x `shouldBe` insert "1" 1 (singleton "2" 2)

    it "filterHmM" $ do
        x <- filterHmM (pure . snd) $ insert "a" True $ singleton "b" False
        x `shouldBe` singleton "a" True

    it "pickKeys" $ do
        let b = singleton "b" "B"
        pickKeys ["b"] (insert "a" "A" b) `shouldBe` b

    -- it "while" $
    --     while ((< 5) . length) (\ls -> ((([head ls] :: String) ++ (ls :: String))) :: String) "ah" `shouldBe` "aaaah"

    it "randomSplit" $ do
        let (train, _validation, _test) = randomSplit (0.5, 0.3, 0.2) [0 .. 9 :: Int]
        length train `shouldBe` 5

hint :: Test
hint = let
        bl = tyCon "Bool"
    in TestList

    [ TestLabel "runInterpreter_" $ TestCase $ do
        x <- runInterpreter_ $ interpret "\"foo\"" (as :: String)
        fromRight "" x @?= "foo"

    , TestLabel "say" $ TestCase $ do
        x <- runInterpreter_ $ return ()
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
        x <- runInterpreter_ (fnIoPairs 1 (var "not") $ parseExpr "[True, False]")
        fromRight "" x @?= "[(True,Right False),(False,Right True)]"
        q <- runInterpreter_ (fnIoPairs 2 (parseExpr "(+)") $ parseExpr "[(1,2),(3,4)]")
        fromRight "" q @?= "[((1,2),Right 3),((3,4),Right 7)]"

    , TestLabel "genInputs" $ TestCase $ do
        -- Bool
        x <- runInterpreter_ (genInputs 10 bl)
        pp <$> fromRight undefined x `shouldContain` ["True"]
        -- -- id
        -- let a = tyVar "a"
        -- q <- runInterpreter_ (genInputs 10 $ tyFun a a)
        -- isRight q `shouldBe` True

    , TestLabel "exprType" $ TestCase $ do
        x <- runInterpreter_ (exprType $ parseExpr "True")
        pp (fromRight undefined x) `shouldBe` "Bool"

    ]

types :: Spec
types = parallel $ do

    let bl = tyCon "Bool"
    let int_ = tyCon "Int"
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
        fnTypeIO (tyFun a b) `shouldBe` ([a], b)

    it "findTypeVars" $ do
        -- Num a => a -> Set b
        let a = tyVar "a"
        let tp = tyForall Nothing (Just $ cxTuple [classA (qName "Num") [a]]) $ tyFun a $ tyApp (tyCon "Set") $ tyVar "b"
        findTypeVars tp `shouldBe` insert "a" [tyCon "Num"] (singleton "b" [])
        -- Ord a => [a] -> [a]
        findTypeVars (tyForall Nothing (Just $ cxTuple [classA (qName "Ord") [a]]) $ tyFun (tyList a) $ tyList a) `shouldBe` singleton "a" [tyCon "Ord"]

    it "randomType" $ do
        tp <- randomType False False nestLimit empty 0
        [tyCon "Bool", tyCon "Int"] `shouldContain` [tp]

    it "randomFnType" $ do
        tp <- randomFnType False False nestLimit empty 0
        [tyFun bl bl, tyFun bl int_, tyFun int_ bl, tyFun int_ int_] `shouldContain` [tp]

    it "genTypes" $ do
        tps <- nub . flatten <$> genTypes 0 10
        tps `shouldContain` [bl]

    fit "fillTypeVars" $ do
        let a = tyVar "a"
        -- int_ -> a: a => Bool
        pp (fillTypeVars (tyFun int_ a) (singleton "a" bl)) `shouldBe` pp (tyFun int_ bl)
        -- Ord a => [a] -> [a]
        let tp = tyForall Nothing (Just $ cxTuple [classA (qName "Ord") [a]]) $ tyFun (tyList a) $ tyList a
        pp (fillTypeVars tp (singleton "a" bl)) `shouldBe` pp (tyFun (tyList bl) (tyList bl))

    it "mergeTyVars" $
        mergeTyVars (singleton "a" [bl, str]) (singleton "a" [int_, bl]) `shouldBe` singleton "a" [bl, str, int_]
    
    it "parseExpr" $
        pp (parseExpr "a") `shouldBe` "a"

    it "parseType" $
        pp (parseType "a") `shouldBe` "a"

    it "isFn" $ do
        isFn bl `shouldBe` False
        isFn (tyVar "a") `shouldBe` False
        isFn (tyFun bl int_) `shouldBe` True

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

    it "genUncurry" $
        pp (genUncurry 2) `shouldBe` "\\ fn (a, b) -> fn a b"

gen :: Test
gen = let 
        bl = tyCon "Bool"
        int_ = tyCon "Int"
        tp = tyFun bl bl
        maybeList = fmap $ \case
                    Right ls -> ls
                    Left _ -> []
        right = fromRight undefined
    in TestList

    [ TestLabel "fnOutputs" $ TestCase $ do
        -- not
        io1 <- runInterpreter_ $ fnOutputs (singleton bl [con "True", con "False"]) (var "not") [[bl]]
        let hm1 = case io1 of
                    Right m -> m
                    Left _ -> empty
        hm1 `shouldBe` singleton [bl] "[(True,Right False),(False,Right True)]"
        -- (+)
        io2 <- runInterpreter_ $ fnOutputs (singleton int_ (int <$> [1,2,3])) (parseExpr "(+)") [[int_,int_]]
        let hm2 = case io2 of
                    Right m -> m
                    Left _ -> empty
        hm2 `shouldBe` singleton [int_,int_] "[((1,1),Right 2),((1,2),Right 3),((1,3),Right 4),((2,1),Right 3),((2,2),Right 4),((2,3),Right 5),((3,1),Right 4),((3,2),Right 5),((3,3),Right 6)]"

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
        -- a => Set a
        let set_ s = tyApp (tyCon "Set") $ tyCon s
        l1 <- maybeList . runInterpreter_ $ instantiateTypes [bl, int_] (tyApp (tyCon "Set") $ tyVar "b")
        (pp <$> l1) `shouldContain` (pp <$> [set_ "Bool", set_ "Int"])
        -- Num a => a -> a -> a
        let a = tyVar "a"
        l2 <- maybeList . runInterpreter_ $ instantiateTypes [bl, int_] (tyForall Nothing (Just $ cxTuple [classA (qName "Num") [a]]) $ tyFun a $ tyFun a a)
        (pp <$> l2) `shouldBe` (pp <$> [tyFun int_ $ tyFun int_ int_])
        -- Ord a => [a] -> [a]
        l3 <- maybeList . runInterpreter_ $ instantiateTypes [bl, int_] (tyForall Nothing (Just $ cxTuple [classA (qName "Ord") [a]]) $ tyFun (tyList a) $ tyList a)
        (pp <$> l3) `shouldBe` (pp <$> [tyFun (tyList bl) $ tyList bl, tyFun (tyList int_) $ tyList int_])

    , TestLabel "instantiateTypeVars" $ TestCase $ do
        -- without type constraint
        l1 <- maybeList . runInterpreter_ $ instantiateTypeVars [bl, int_] $ singleton "a" []
        l1 `shouldBe` [singleton "a" bl, singleton "a" int_]
        -- with type constraint
        l2 <- maybeList . runInterpreter_ $ instantiateTypeVars [bl, int_] $ singleton "a" [tyCon "Num"]
        l2 `shouldBe` [singleton "a" int_]
        -- Ord a => [a] -> [a]
        l3 <- maybeList . runInterpreter_ $ instantiateTypeVars [bl, int_] $ singleton "a" [tyCon "Ord"]
        l3 `shouldBe` [singleton "a" bl, singleton "a" int_]

    , TestLabel "typeRelation" $ TestCase $ do
        let a = tyVar "a"
        -- crap, I cannot test NEQ as it explodes, while LT/GT seem to imply constraints/forall...
        q <- runInterpreter_ $ typeRelation int_ a
        right q `shouldBe` EQ
        w <- runInterpreter_ $ typeRelation a bl
        right w `shouldBe` EQ
        e <- runInterpreter_ $ typeRelation bl bl
        right e `shouldBe` EQ
        r <- runInterpreter_ $ typeRelation a a
        right r `shouldBe` EQ
        t <- runInterpreter_ $ typeRelation a $ tyVar "b"
        right t `shouldBe` EQ

    , TestLabel "matchesType" $ TestCase $ do
        let a = tyVar "a"
        q <- runInterpreter_ $ matchesType int_ a
        right q `shouldBe` True
        w <- runInterpreter_ $ matchesType a bl
        right w `shouldBe` True
        e <- runInterpreter_ $ matchesType bl bl
        right e `shouldBe` True
        r <- runInterpreter_ $ matchesType bl int_
        right r `shouldBe` False
        t <- runInterpreter_ $ matchesType a a
        right t `shouldBe` True
        y <- runInterpreter_ $ matchesType a $ tyVar "b"
        right y `shouldBe` True

    , TestLabel "matchesConstraints" $ TestCase $ do
        x <- runInterpreter_ $ matchesConstraints int_ [tyCon "Enum"]
        fromRight False x `shouldBe` True

    -- , TestLabel "filterTypeSigIoFnsM" $ TestCase $ do
    --     let fn_asts = insert "not" (var "not") $ singleton "not_" $ app (var "id") $ var "not"
    --     let hm = filterTypeSigIoFnsM fn_asts (singleton "Bool -> Bool" $ singleton "[(True, False), (False, True)]" ["not", "not_"])
    --     hm `shouldBe` singleton "Bool -> Bool" (singleton "[(True, False), (False, True)]" "not")
    --     -- TODO: test generic functions get priority

    ]
