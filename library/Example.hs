-- {-# LANGUAGE ScopedTypeVariables #-}
-- TemplateHaskell, QuasiQuotes

-- | An example module.
module Example (main) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Syntax ( Exp, Type(TyCon, TyVar) ) -- , SpecialCon(ExprHole), Name(Ident), QName(UnQual), TyApp, TyFun, Exp(ExpTypeSig)
-- import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.SrcLoc ( SrcSpanInfo ) -- , SrcSpan(..), SrcSpanInfo(srcInfoSpan, srcInfoPoints)
-- import Test.QuickCheck ( Gen, arbitrary, sample, sample', variant, generate, resize )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, interpret, as, typeOf, runInterpreter, lift, setImports) -- , MonadInterpreter, infer, eval, kindOf, typeChecks, InterpreterError(..), GhcError(..), liftIO
import Data.List (nub, replicate) -- intercalate, 
-- import System.Random (randomRIO)
import Control.Monad (forM_)
import Utility (Item(..), say, errorString, interpretIO, fnStr, returnType, randomType, fnInTp, skeleton, flatten)
-- , pick, typeNode, polyTypeNode, undef

-- | An example function.
main :: IO ()
main = do
    r <- runInterpreter testHint
    case r of
        Left err -> putStrLn $ errorString err
        Right () -> return ()

testHint :: Interpreter ()
testHint = do
    let modules = ["Prelude", "Data.List", "Test.QuickCheck"]
    setImports modules

    -- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
    -- let src = "\\b -> let _b = (b :: Bool) in not b"
    let src = "id"

    (hole_expr, triplets) <- fromFn src
    say $ src
    say $ prettyPrint hole_expr
    forM_ triplets $ \trplt -> do
        let (in_tp_str, out_tp_str, io_pairs) = trplt
        say $ fnStr in_tp_str out_tp_str
        say io_pairs
    return ()

-- TODO: do sample generation not for each function but for each function type?
-- TODO: deduplicate functions by identical types + io, keeping the shortest
fromFn :: String -> Interpreter (Exp SrcSpanInfo, [] (String, String, String))
fromFn fn_str = do
    fn_tp_str <- typeOf fn_str
    in_tp <- fnInTp fn_tp_str
    nested_types <- lift $ instantiateType in_tp
    let in_types = nub $ flatten nested_types
    triplets <- mapM (handleInTp fn_tp_str fn_str) in_types
    --  :: [] (String, String, String)
    let hole_expr = skeleton fn_tp_str
    return (hole_expr, triplets)

handleInTp :: String -> String -> (Type SrcSpanInfo) -> Interpreter (String, String, String)
handleInTp fn_tp_str fn_str in_type = do
    let in_tp_str = prettyPrint in_type
    ins <- interpretIO $ "let n = 10; seed = 0 in show <$> nub <$> sample' (resize n $ variant seed arbitrary :: Gen (" ++ in_tp_str ++ "))"
    -- TODO: evaluate function calls from AST i/o from interpreter
    io_pairs <- interpret ("show $ zip (" ++ ins ++ ") $ (" ++ fn_str ++ ") <$> " ++ ins) (as :: String)
    out_tp_str <- returnType fn_tp_str in_tp_str
    return (in_tp_str, out_tp_str, io_pairs)

-- TODO: this isn't right, I shouldn't replace each type variable instance, but find the type variables and their occurrences, then for each one (e.g. a) instantiate types and substitute all occurrences for these.
instantiateType :: (Type SrcSpanInfo) -> IO (Item (Type SrcSpanInfo))
instantiateType tp = case tp of
                        TyCon _l qname -> return $ One $ [TyCon _l qname]
                        TyVar _l _name -> (Many . fmap (One . pure)) <$> (mapM id $ replicate maxInstances $ randomType nestLimit)
                        -- TyApp _l a b -> Many [instantiateType ?]
                        x -> fail $ "unexpected " ++ show x
                      where
                          maxInstances = 5  -- may get less after nub filters out duplicate type instances
                          nestLimit = 2

-- -- Bool/Int types are bs substitutes for a/b to statically test if this compiles for above
-- genIO :: (Bool -> Int) -> IO String
-- genIO fn = do
--     ins <- let n = 10; seed = 0 in nub <$> sample' (resize n $ variant seed arbitrary :: Gen (Bool))
--     return $ show $ zip ins $ (fn) <$> ins
