{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

-- Tasty: <http://documentup.com/feuerbach/tasty>
import           Test.Tasty                   (TestTree, defaultMain, testGroup)
-- Hspec: <https://hspec.github.io>
import           Test.HUnit.Base              (Test (..))
import           Test.HUnit.Text              (runTestTT)
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit             ((@?=))

import           Control.Exception            (SomeException, try, evaluate)
-- import           Data.Word                    (Word64)
import           Data.Either                  (fromRight, isRight)
import           Data.Functor                 (void, (<&>))
-- import           Data.Bifunctor               (first)
import           Data.HashMap.Lazy            (HashMap, empty, insert, singleton, (!))
import qualified Data.Set
import           System.Random                (StdGen, mkStdGen)
import           Language.Haskell.Interpreter (as, interpret, liftIO) -- , typeChecksWithDetails
import           Util                         (fstOf3)

-- import           GHC.Exts
-- import           GHC.TypeNats
import           GHC.TypeNats (Mod, type (*))  -- , KnownNat, Nat, Div, type (+), type (-)
-- import           Torch.Random (Generator)
-- import           Torch.Functional.Internal (gather)
-- import qualified Torch.DType                   as D
import qualified Torch.Tensor                  as D
-- import qualified Torch.Device                  as D
-- import qualified Torch.Random                  as D
import qualified Torch.Functional.Internal as I
import qualified Torch.Functional              as F
import qualified Torch.NN                      as A
import           Torch.Typed.Aux
-- import           Torch.TensorOptions
import           Torch.Typed.Tensor
import           Torch.Typed.Factories

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
import qualified Synthesis.Synthesizer.Encoder as Enc
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.NSPS
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical as Categorical

main ∷ IO ()
main = do
    -- unlike Tasy, HUnit's default printer is illegible,
    -- but helps ensure the Interpreter is run only once...
    void $ runTestTT $ TestList [hint, gen]

    -- Tasty HSpec
    util_ <- testSpec "Utility" util
    types_ <- testSpec "Types" types
    typeGen_ <- testSpec "TypeGen" typeGen
    find_ <- testSpec "FindHoles" find
    ast_ <- testSpec "Ast" ast
    synthesizer_ <- testSpec "Synthesizer" synthesizer
    let tree :: TestTree = testGroup "synthesis" [util_, types_, typeGen_, find_, ast_, synthesizer_]
    defaultMain tree

util ∷ Spec
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

    -- it "flattenTuple" $
    --     flattenTuple (DeepTuple (1, SingleTuple (2, 3))) `shouldBe` [1 :: Int, 2, 3]

    it "pick" $ do
        x <- pick [1 :: Int, 2, 3]
        x < 5 `shouldBe` True

    it "groupByVal" $
        groupByVal [(1 :: Int, "odd"), (2, "even"), (3, "odd")] `shouldBe` (insert "odd" [1, 3] (singleton "even" [2]) :: HashMap String [Int])

    it "fromKeys" $
        fromKeys show [1 :: Int, 2] `shouldBe` insert 1 "1" (singleton 2 "2")

    -- it "fromVals" $
    --     fromVals show [1 :: Int, 2] `shouldBe` insert "1" 1 (singleton "2" 2)

    it "fromKeysM" $ do
        x <- fromKeysM (pure . show) [1 :: Int, 2]
        x `shouldBe` insert 1 "1" (singleton 2 "2")

    it "fromValsM" $ do
        x <- fromValsM (pure . show) [1 :: Int, 2]
        x `shouldBe` insert "1" 1 (singleton "2" 2)

    -- it "filterHmM" $ do
    --     x <- filterHmM (pure . snd) $ insert "a" True $ singleton "b" False
    --     x `shouldBe` singleton "a" True

    it "pickKeys" $ do
        let b = singleton "b" "B"
        pickKeys ["b"] (insert "a" "A" b) `shouldBe` b

    it "randomSplit" $ do
        GenerationConfig {..} :: GenerationConfig <- liftIO parseGenerationConfig
        let stdGen :: StdGen = mkStdGen seed
        let (nums_train, _nums_validation, _nums_test) =
                randomSplit stdGen (0.5, 0.3, 0.2) [0 .. 9 :: Int]
        length nums_train `shouldBe` 5

    -- it "batchList" $ do
    --     batchList 2 [1,2,3,4,5 :: Int] `shouldBe` [[1,2],[3,4],[5]]

    -- it "statistic" $ do
    --     statistic (0 :: Int) (+) (const id) [1,2,3 :: Int] `shouldBe` 6
    --     statistic (0 :: Int) (+) (\ xs i -> i `div` length xs) [1,2,3 :: Int] `shouldBe` 2
    --     -- statistic (stat { acc = 0 :: Int, sufficientStatistic = (+) }) [1,2,3 :: Int] `shouldBe` 6
    --     -- statistic (stat { acc = 0 :: Int, sufficientStatistic = (+), summarizer = (\ xs i -> i / length xs) }) [1,2,3 :: Int] `shouldBe` 2

hint ∷ Test
hint = let
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
        GenerationConfig { crashOnError = crashOnError } :: GenerationConfig <- parseGenerationConfig
        x <- interpretUnsafe (fnIoPairs crashOnError 1 (var "not") $ parseExpr "[True, False]")
        pp_ x @?= pp_ ([(parseExpr "True", Right (parseExpr "False")), (parseExpr "False", Right (parseExpr "True"))] :: [(Expr, Either String Expr)])
        q <- interpretUnsafe (fnIoPairs crashOnError 2 (parseExpr "(+)") $ parseExpr "[(1,2),(3,4)]")
        pp_ q @?= pp_ ([(parseExpr "(1, 2)", Right (parseExpr "3")), (parseExpr "(3, 4)", Right (parseExpr "7"))] :: [(Expr, Either String Expr)])

    , TestLabel "exprType" $ TestCase $ do
        x <- interpretUnsafe (exprType $ parseExpr "True")
        pp x `shouldBe` "Bool"

    ]

types ∷ Spec
types = parallel $ let
        bl = tyCon "Bool"
        int_ = tyCon "Int"
        -- str = tyCon "String"
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
        -- pp (expTypeSig holeExpr $ tyCon "Int") `shouldBe` "_ :: Int"
        pp (expTypeSig holeExpr $ tyCon "Int") `shouldBe` "undefined :: Int"

    it "fnTypeIO" $ do
        let a = tyVar "a"
        let b = tyVar "b"
        fnTypeIO (tyFun a b) `shouldBe` ([a], b)

    it "parseExpr" $
        pp (parseExpr "a") `shouldBe` "a"

    it "parseType" $ do
        pp (parseType "a") `shouldBe` "a"
        let s = "(Eq (a -> Bool)) => a"
        either_ :: Either SomeException Tp <- try $ evaluate $ parseType s
        isRight either_ `shouldBe` False

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
        typeSane (tyForall Nothing (Just (cxTuple [typeA (qName "Eq") (tyFun a (tyCon "Bool"))])) a) `shouldBe` False
        -- I guess this means I'd judge HaskTorch's Typed functions as insane, but
        -- for the purpose of program synthesis, for the moment let's say they are.

    -- it "fnTpArity" $ do
    --     fnTpArity bl `shouldBe` 0
    --     fnTpArity (parseType "forall a . (Enum a) => (Int -> Bool) -> a -> Bool") `shouldBe` 2

    -- it "mkExpr" $ do
    --     pp (mkExpr (1 :: Int)) `shouldBe` "1"

    -- it "mkExprPair" $ do
    --     let either' :: Either String Bool = Right True
    --     pp_ (mkExprPair (1 :: Int, either')) `shouldBe` "(\"1\", Right (\"True\"))"

typeGen ∷ Spec
typeGen = parallel $ let
        bl = tyCon "Bool"
        int_ = tyCon "Int"
        str = tyCon "String"
    in do

    it "findTypeVars" $ do
        -- Num a => a -> Set b
        let a = tyVar "a"
        let tp = tyForall Nothing (Just $ cxTuple [typeA (qName "Num") a]) $ tyFun a $ tyApp (tyCon "Set") $ tyVar "b"
        findTypeVars tp `shouldBe` insert "a" (0, [tyCon "Num"]) (singleton "b" (0, []))
        -- Ord a => [a] -> [a]
        findTypeVars (tyForall Nothing (Just $ cxTuple [typeA (qName "Ord") a]) $ tyFun (tyList a) $ tyList a) `shouldBe` singleton "a" (0, [tyCon "Ord"])
        -- Foldable t => t a -> Bool
        pp_ (findTypeVars (parseType "Foldable t => t a -> Bool")) `shouldBe` pp_ (insert "t" (1 :: Int, [tyCon "Foldable"]) (singleton "a" (0 :: Int, [])))

    it "randomType" $ do
        GenerationConfig { nestLimit = nestLimit } :: GenerationConfig <- liftIO parseGenerationConfig
        tp <- randomType False False nestLimit empty 0
        [tyCon "Bool", tyCon "Int"] `shouldContain` [tp]

    it "randomFnType" $ do
        GenerationConfig { nestLimit = nestLimit } :: GenerationConfig <- liftIO parseGenerationConfig
        tp <- randomFnType False False nestLimit empty 0
        [tyFun bl bl, tyFun bl int_, tyFun int_ bl, tyFun int_ int_] `shouldContain` [tp]

    it "genTypes" $ do
        hm <- genTypes 0 10
        hm ! 0 `shouldContain` [bl]

    it "fillTypeVars" $ do
        let a = tyVar "a"
        -- int_ -> a: a => Bool
        pp (fillTypeVars (tyFun int_ a) (singleton "a" bl)) `shouldBe` pp (tyFun int_ bl)
        -- Ord a => [a] -> [a]
        let tp = tyForall Nothing (Just $ cxTuple [typeA (qName "Ord") a]) $ tyFun (tyList a) $ tyList a
        pp (fillTypeVars tp (singleton "a" bl)) `shouldBe` pp (tyFun (tyList bl) (tyList bl))

    it "mergeTyVars" $
        mergeTyVars (singleton "a" [bl, str]) (singleton "a" [int_, bl]) `shouldBe` singleton "a" [bl, str, int_]

find ∷ Spec
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

ast ∷ Spec
ast = parallel $ let
        bl = tyCon "Bool"
        int_ = tyCon "Int"
    in do

    it "skeleton" $ do
        -- pp (skeleton bl) `shouldBe` "_ :: Bool"
        pp (skeleton bl) `shouldBe` "undefined :: Bool"

    it "numAstNodes" $
        numAstNodes holeExpr `shouldBe` 3

    it "hasHoles" $ do
        hasHoles holeExpr `shouldBe` False
        let expr = expTypeSig holeExpr int_
        hasHoles expr `shouldBe` True

    it "genUncurry" $
        pp (genUncurry 2) `shouldBe` "\\ fn (a, b) -> fn a b"

    it "genInputs" $ do
        GenerationConfig { seed = seed
                , numMin = numMin
                , numMax = numMax
                , listMin = listMin
                , listMax = listMax
                } :: GenerationConfig <- liftIO parseGenerationConfig
        let stdGen :: StdGen = mkStdGen seed
        let intRange = (numMin, numMax)
        let listLengths = (listMin, listMax)
        -- Bool
        pp <$> (genInputs stdGen intRange listLengths 10 bl) `shouldContain` ["True"]
        -- [Bool]
        let lists = genInputs stdGen intRange listLengths 10 $ tyList bl
        (length . nubPp . concat . fmap unList) lists `shouldBe` 2

    it "genHoledVariants" $ do
        let tp = parseType "Int -> String -> Tp"
        fmap pp (genHoledVariants 0 "f" tp) `shouldBe` ["f", "f (undefined :: Int)", "f (undefined :: Int) (undefined :: String)"]

gen ∷ Test
gen = let
        bl = tyCon "Bool"
        int_ = tyCon "Int"
        tp = tyFun bl bl
    in TestList

    [ TestLabel "fnOutputs" $ TestCase $ do
        GenerationConfig { crashOnError = crashOnError } :: GenerationConfig <- parseGenerationConfig
        -- not
        hm1 <- interpretUnsafe $ fnOutputs crashOnError (singleton bl [con "True", con "False"]) (var "not") [[bl]]
        pp_ hm1 `shouldBe` pp_ ((singleton [bl] [(parseExpr "True", Right (parseExpr "False")), (parseExpr "False", Right (parseExpr "True"))]) :: HashMap [Tp] [(Expr, Either String Expr)])
        -- (+)
        hm2 <- interpretUnsafe $ fnOutputs crashOnError (singleton int_ (int <$> [1,2,3])) (parseExpr "(+)") [[int_,int_]]
        pp_ hm2 `shouldBe` pp_ ((singleton [int_,int_] [(parseExpr "(1,1)", Right (parseExpr "2")), (parseExpr "(1,2)", Right (parseExpr "3")), (parseExpr "(1,3)", Right (parseExpr "4")), (parseExpr "(2,1)", Right (parseExpr "3")), (parseExpr "(2,2)", Right (parseExpr "4")), (parseExpr "(2,3)", Right (parseExpr "5")), (parseExpr "(3,1)", Right (parseExpr "4")), (parseExpr "(3,2)", Right (parseExpr "5")), (parseExpr "(3,3)", Right (parseExpr "6"))]) :: HashMap [Tp] [(Expr, Either String Expr)])

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

    , TestLabel "genFn" $ TestCase $ do
        let blockAsts' = singleton "not_" $ var "not"
        fn <- interpretUnsafe $ genFn 0 [("not_", var "not_")] blockAsts'
        is_fn <- interpretUnsafe $ isFn <$> exprType fn
        is_fn `shouldBe` True

    , TestLabel "genFns" $ TestCase $ do
        let blockAsts' = singleton "not_" $ var "not"
        lst <- interpretUnsafe $ genFns 0 [("not_", var "not_")] blockAsts'
        (gtrExpr <$> lst) `shouldContain` [var "not_"]

    , TestLabel "instantiateTypes" $ TestCase $ do
        let lst_ = tyCon "[]"
        -- a => Set a
        let set_ s = tyApp (tyCon "Set") $ tyCon s
        l1 <- interpretUnsafe $ instantiateTypes (singleton 0 [bl, int_]) (tyApp (tyCon "Set") $ tyVar "b")
        (pp <$> l1) `shouldContain` (pp <$> [set_ "Bool", set_ "Int"])
        -- Num a => a -> a -> a
        let a = tyVar "a"
        l2 <- interpretUnsafe $ instantiateTypes (singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA (qName "Num") a]) $ tyFun a $ tyFun a a)
        (pp <$> l2) `shouldBe` (pp <$> [tyFun int_ $ tyFun int_ int_])
        -- Ord a => [a] -> [a]
        l3 <- interpretUnsafe $ instantiateTypes (singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA (qName "Ord") a]) $ tyFun (tyList a) $ tyList a)
        (pp <$> l3) `shouldBe` (pp <$> [tyFun (tyList bl) $ tyList bl, tyFun (tyList int_) $ tyList int_])
        -- Foldable t => t Bool -> Bool
        l4 <- interpretUnsafe $ instantiateTypes (insert 1 [lst_] $ singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA (qName "Foldable") a]) $ tyFun (tyApp a bl) bl)
        (pp <$> l4) `shouldBe` (pp <$> [tyFun (tyApp lst_ bl) bl])
        -- Foldable t => t a -> Bool
        let t = tyVar "t"
        l5 <- interpretUnsafe $ instantiateTypes (insert 1 [lst_] $ singleton 0 [bl, int_]) (tyForall Nothing (Just $ cxTuple [typeA (qName "Foldable") t]) $ tyFun (tyApp t a) bl)
        (pp <$> l5) `shouldBe` (pp <$> [tyFun (tyApp lst_ bl) bl, tyFun (tyApp lst_ int_) bl])

    , TestLabel "instantiateTypeVars" $ TestCase $ do
        let lst_ = tyCon "[]"
        -- without type constraint
        l1 <- interpretUnsafe $ instantiateTypeVars (singleton 0 [bl, int_]) $ singleton "a" (0, [])
        l1 `shouldBe` [singleton "a" bl, singleton "a" int_]
        -- with type constraint
        l2 <- interpretUnsafe $ instantiateTypeVars (singleton 0 [bl, int_]) $ singleton "a" (0, [tyCon "Num"])
        l2 `shouldBe` [singleton "a" int_]
        -- Ord a => [a] -> [a]
        l3 <- interpretUnsafe $ instantiateTypeVars (singleton 0 [bl, int_]) $ singleton "a" (0, [tyCon "Ord"])
        l3 `shouldBe` [singleton "a" bl, singleton "a" int_]
        -- Foldable t => t Bool -> Bool
        l4 <- interpretUnsafe $ instantiateTypeVars (insert 1 [lst_] $ singleton 0 [bl, int_]) $ singleton "t" (1, [tyCon "Foldable"])
        l4 `shouldBe` [singleton "t" lst_]
        -- Foldable t => t a -> Bool
        l5 <- interpretUnsafe $ instantiateTypeVars (insert 1 [lst_] $ singleton 0 [bl, int_]) $ insert "a" (0, []) $ singleton "t" (1, [tyCon "Foldable"])
        pp_ l5 `shouldBe` pp_ [insert "a" bl (singleton "t" lst_), insert "a" int_ (singleton "t" lst_)]

    , TestLabel "typeRelation" $ TestCase $ do
        let a = tyVar "a"
        -- crap, I cannot test NEQ as it explodes, while LT/GT seem to imply constraints/forall...
        q <- interpretUnsafe $ typeRelation int_ a
        q `shouldBe` EQ
        w <- interpretUnsafe $ typeRelation a bl
        w `shouldBe` EQ
        e <- interpretUnsafe $ typeRelation bl bl
        e `shouldBe` EQ
        r <- interpretUnsafe $ typeRelation a a
        r `shouldBe` EQ
        t <- interpretUnsafe $ typeRelation a $ tyVar "b"
        t `shouldBe` EQ

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

    -- , TestLabel "filterTypeSigIoFnsM" $ TestCase $ do
    --     let fn_asts = insert "not" (var "not") $ singleton "not_" $ app (var "id") $ var "not"
    --     let hm = filterTypeSigIoFnsM fn_asts (singleton "Bool -> Bool" $ singleton "[(True, False), (False, True)]" ["not", "not_"])
    --     hm `shouldBe` singleton "Bool -> Bool" (singleton "[(True, False), (False, True)]" "not")
    --     -- TODO: test to ensure generic functions get priority

    ]

-- dummy values to trick the compiler
type T = 42

synthesizer ∷ Spec
synthesizer = parallel $ do

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
        -- let r :: Tensor Dev 'D.Float '[2] = UnsafeMkTensor . D.asTensor $ [10.0,20.0::Float]
        -- rotateT r `shouldBe` ?

    it "nsps" $ do
        let batch_size :: Int = natValI @BatchSize
        -- let seed' :: Int = 123
        let dsl = blockAsts
        variants :: [(String, Expr)] <- interpretUnsafe $ dslVariants dsl
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "[]")), (parseExpr "1", Right (parseExpr "[True]")), (parseExpr "2", Right (parseExpr "[True, True]"))]
        init_enc_model :: BaselineMLPEncoder <- A.sample $ BaselineMLPEncoderSpec max_char h0 h1 $ dirs * Enc.h
        io_feats :: Tnsr '[BatchSize, 2 * Dirs * Enc.H * T] <- baselineMLPEncoder init_enc_model io_pairs
        let ppt :: Expr = parseExpr "not (not (undefined :: Bool))"
        init_r3nn_model :: R3NN M Rules <- A.sample $ initR3nn @M @Rules @T variants batch_size
        hole_expansion_probs <- runR3nn @T init_r3nn_model ppt io_feats
        (_zero, ppt') <- predictHole variants ppt hole_expansion_probs
        putStrLn $ pp ppt'
        -- hasHoles ppt' `shouldBe` False
    
    it "transfer lenses" $ do
        let xpr  = parseExpr "(undefined :: a) (bar (undefined :: b))"
        let xpr' = parseExpr "foo (baz boo)"
        let getters = fst <$> findHolesExpr xpr
        length getters `shouldBe` 2
        let [gtr1, gtr2] = getters
        pp (gtr1 xpr') `shouldBe` "foo"
        pp (gtr2 xpr') `shouldBe` "boo"

    it "categorical" $ do
        t :: Tnsr '[2, 3] <- abs <$> randn
        x <- Distribution.sample (Categorical.fromProbs $ toDynamic t) [1]
        D.shape x `shouldBe` [1,2]

    it "sampleIdxs" $ do
        let t = D.asTensor $ [[0.0, 0.0], [1.0, 0.0 :: Float]]
        idxs <- sampleIdxs t
        D.asValue (foldl (\ t' idx -> D.select t' 0 idx) t idxs) `shouldBe` (1.0 :: Float)
