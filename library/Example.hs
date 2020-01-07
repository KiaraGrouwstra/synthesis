{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, FlexibleInstances, TypeFamilies, Rank2Types #-}
-- , OverlappingInstances

-- | An example module.
module Example (main) where

import Language.Haskell.Exts.Build --( wildcard, intE )
import Language.Haskell.Exts.Pretty --( prettyPrint )
import Language.Haskell.Exts.Syntax ( Exp(ExpTypeSig), SpecialCon(ExprHole), Type(TyFun, TyCon, TyApp, TyVar), Name(Symbol, Ident), QName(UnQual) )
import Language.Haskell.Exts.Parser --( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.SrcLoc --( SrcSpanInfo )
import qualified Data.Typeable -- ( TypeRep, typeOf, funResultTy, typeRepArgs )
import Test.QuickCheck -- ( Gen, arbitrary, sample, sample', variant, generate )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter -- (Interpreter, MonadInterpreter, InterpreterError(..), GhcError, interpret, as, infer, eval, typeOf, typeChecks, kindOf, runInterpreter)
import Language.Haskell.Interpreter.Unsafe (unsafeInterpret)
import Data.List (intercalate, nub)
import System.Random (randomRIO)

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

-- | An example function.
main :: IO ()
main = do
    r <- runInterpreter testHint
    case r of
        Left err -> putStrLn $ errorString err
        Right () -> return ()

say :: String -> Interpreter ()
say = liftIO . putStrLn

testHint :: Interpreter ()
testHint = do
    -- set [languageExtensions := [ScopedTypeVariables]]
    let modules = ["Prelude", "Data.List", "Test.QuickCheck"]
    setImports modules

    -- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
    -- let src = "\\b -> let _b = (b :: Bool) in not b"
    let src = "id"
    -- let expr = fromParseResult (parse src :: ParseResult (Exp SrcSpanInfo))

    -- in_tp_str, out_tp_str, 
    (hole_expr, triplets) <- from_fn src -- expr
    -- say $ show (in_tp_str, out_tp_str)
    say $ src
    say $ prettyPrint hole_expr
    -- say $ io_pairs
    -- do (in_tp_str, out_tp_str, io_pairs) <- triplets
    --     say (in_tp_str, out_tp_str)
    --     say io_pairs
    --     return ()
    -- do trplt <- triplets
    --     say trplt
    --     return ()
    say <$> triplets
    return ()

-- Exp SrcSpanInfo
-- String, String, 
-- TODO: do sample generation not for each function level but for each function type?
from_fn :: String -> Interpreter (Exp SrcSpanInfo, [] (String, String, String))
from_fn fn_str = do  -- expr
    -- let fn_str = prettyPrint expr
    fn_tp_str <- typeOf fn_str  -- cannot do id?
    let hole_expr = skeleton fn_tp_str
    in_tp <- fn_in_tp fn_tp_str
    -- checking if the input type *is* a type variable -- what about nested occurrences?
    let random_types = 5
    let in_types = nub $ flatten $ case in_tp of
                                    TyCon _l qname -> [prettyPrint qname]
                                    TyVar _l _name -> [random_type]
                                    -- TyApp _l a b -> ?
    let triplets = handle_in_tp fn_str fn_tp_str <$> in_types
    return (hole_expr, triplets)

handle_in_tp :: String -> String -> (Type SrcSpanInfo) -> Interpreter (String, String, String)
handle_in_tp fn_str fn_tp_str in_type = do
    let in_tp_str = prettyPrint <$> in_type
    let cmd :: String = "do \n\
    \    let seed = 0 -- somehow this won't make it deterministic? \n\
    \    let n = 10 \n\
    \    ins <- nub <$> sample' (resize n $ variant seed arbitrary :: Gen " ++ in_tp_str ++ ") \n\
    \    let outs = (" ++ fn_str ++ ") <$> ins \n\
    \    return $ show $ zip ins outs \n\
    \"
    io <- interpret cmd (as :: IO String)
    io_pairs <- lift io
    out_tp_str <- returnType fn_tp_str in_tp_str
    return (in_tp_str, out_tp_str, io_pairs)

-- -- Bool/Int types are bs substitutes for a/b to statically test if this compiles for above
-- gen_io :: Bool -> (Bool -> Int) -> IO String
-- gen_io tp fn = do
--     let seed = 0 -- somehow this won't make it deterministic?
--     let n = 10
--     ins <- nub <$> sample' (resize n $ variant seed arbitrary :: Gen Bool)
--     let outs = fn <$> ins
--     return $ show $ zip ins outs

-- str = show $ funResultTy (typeOf (reverse :: [Char] -> [Char])) $ typeOf "abc"
returnType :: String -> String -> Interpreter String
returnType fn_tp_str par_tp_str = typeOf $ "(undefined :: " ++ fn_tp_str ++ ") (undefined :: " ++ par_tp_str ++ ")"

fn_in_tp :: String -> Interpreter (Type SrcSpanInfo)
fn_in_tp fn_tp_str = do
    let tp_ast = fromParseResult (parse ("_ :: " ++ fn_tp_str) :: ParseResult (Exp SrcSpanInfo))
    let tp_fn = case tp_ast of
                    ExpTypeSig _mdl _exp tp -> tp
    let in_tp = case tp_fn of
                    TyFun _mdl i _o -> i
    say $ prettyPrint in_tp
    return in_tp
 
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

random_type :: IO (Type SrcSpanInfo)
random_type = do
    io <- pick [
                simple "Bool",
                simple "Int",
                mono "[]"
            ]
    io
    where
        simple = pure . type_node
        mono str = do
            tp <- random_type
            return $ poly_type_node str tp

l :: SrcSpanInfo
l = SrcSpanInfo {srcInfoSpan = span, srcInfoPoints = []}  -- [span]
    where 
        span = SrcSpan "<unknown>.hs" 1 1 1 1

type_node :: String -> (Type SrcSpanInfo)
type_node str = TyCon l $ UnQual l $ Ident l str

poly_type_node :: String -> (Type SrcSpanInfo) -> (Type SrcSpanInfo)
poly_type_node str tp = TyApp l (type_node str) tp

-- -- can't get TypeRep for polymorphic types
-- skeleton :: TypeRep -> Exp SrcSpanInfo
-- skeleton rep = expr
--     where
--         io = typeRepArgs rep
--         hole = Var l $ Special l $ ExprHole l
--         i = type_node . show $ head io
--         o = type_node . show $ last io
--         tp_fn = TyFun l i o
--         expr = ExpTypeSig l hole tp_fn

skeleton :: String -> Exp SrcSpanInfo
skeleton fn_tp_str = expr
    where
        src = "_ :: " ++ fn_tp_str
        expr = fromParseResult (parse src :: ParseResult (Exp SrcSpanInfo))
