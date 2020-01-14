{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, DataKinds #-}

-- | main logic
module Example (main) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
-- import Language.Haskell.Exts.Syntax ( Type(TyFun) )
-- import Test.QuickCheck ( Gen, arbitrary, sample, sample', variant, generate, resize )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, typeOf, runInterpreter, setImports, lift)
-- , interpret, as, lift
import Data.List (nub)
import Control.Monad (forM_)
import Hint (say, errorString, genInputs)
import Ast (fnOutputs, filterTypeSigIoFns, fillHole, skeleton, nestLimit, maxInstances)
-- , instantiateFnTypes
import Types (Tp, Expr, fnTypeIO, instantiateTypes, genTypes)
import Utility (groupByVal, flatten, toMapBy)
import Data.HashMap.Lazy (HashMap, empty, insert, elems, (!), mapWithKey, fromList, toList)
import Debug.Dump (d)

-- | An example function.
main :: IO ()
main = do
    r <- runInterpreter testHint
    case r of
        Left err -> putStrLn $ errorString err
        Right () -> return ()

modules :: [String]
modules = ["Prelude", "Data.List", "Test.QuickCheck"]

-- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
fnBodies :: HashMap String String
fnBodies = insert "not_" "\\b -> let _b = (b :: Bool) in not b" $
                insert "not" "not" $
                insert "id" "id"
                empty

-- TODO: polyary functions
testHint :: Interpreter ()
testHint = do
    setImports modules
    let fn_asts :: HashMap String Expr = (\str -> fromParseResult (parse str :: ParseResult Expr)) <$> fnBodies
    fn_type_strs :: HashMap String String <- mapM typeOf fnBodies

    -- fn_type_instantiations :: HashMap String [Tp] <- instantiateFnTypes fn_type_strs
    let fn_types :: HashMap String Tp = (\type_str -> fromParseResult (parse type_str :: ParseResult Tp)) <$> fn_type_strs
    fill_types :: [Tp] <- nub . flatten <$> lift (genTypes nestLimit maxInstances)
    let fn_type_instantiations :: HashMap String [Tp] = instantiateTypes fill_types <$> fn_types
    let fn_io_type_instantiations :: HashMap String [(Tp, Tp)] = fmap fnTypeIO <$> fn_type_instantiations
    let fn_in_str_type_instantiations :: HashMap String [String] = fmap (prettyPrint . fst) <$> fn_io_type_instantiations

    -- casting keys to string cuz Type isn't hashable 😐
    -- let in_type_instantiations :: HashMap String [Tp] = fromList $ zip str_in_types $ nub . flatten <$> in_type_instantiations_
    -- let type_instantiations :: [Tp] = nub $ concat $ elems in_type_instantiations
    let in_type_instantiations :: [Tp] = nub . fmap fst . concat . elems $ fn_io_type_instantiations
    let str_instantiations :: [String] = prettyPrint <$> in_type_instantiations
    -- let str_in_type_instantiations :: HashMap String [String] = fmap prettyPrint <$> in_type_instantiations
    -- do sample generation not for each function but for each function input type
    -- casting keys to string cuz Type isn't hashable 😐
    instantiation_inputs :: HashMap String String <- fromList . zip str_instantiations <$> mapM genInputs str_instantiations
    -- TODO: even detect functions identical in function but different in scope of inputs -- the type hashmap layer now prevents such comparison
    fn_in_type_instance_outputs :: HashMap String (HashMap String String) <- sequence $ mapWithKey (fnOutputs fnBodies instantiation_inputs) fn_in_str_type_instantiations
    -- group functions with identical type signatures
    let type_sig_fns :: HashMap String [String] = groupByVal $ toList fn_type_strs
    -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
    let type_sig_io_fns :: HashMap String (HashMap String [String]) = (\ks -> groupByVal $ zip ks $ show . (!) fn_in_type_instance_outputs <$> ks) <$> type_sig_fns
    -- deduplicate functions by identical types + io, keeping the shortest

    type_sig_io_fns_filtered :: HashMap String (HashMap String String) <- filterTypeSigIoFns fn_asts type_sig_io_fns

    let kept_fns :: [String] = concat $ elems <$> elems type_sig_io_fns_filtered

    say ""
    say "enumerating function i/o examples!"
    forM_ kept_fns $ \k -> do
        say ""
        let fn_type_str = fn_type_strs ! k
        say $ k ++ " :: " ++ fn_type_str
        let instantiations = fn_in_str_type_instantiations ! k
        forM_ instantiations $ \tp -> do
            let io_pairs = fn_in_type_instance_outputs ! k ! tp
            say $ tp ++ ": " ++ io_pairs

    say ""
    say "finding fits!"
    let fn_hole_exprs :: HashMap String Expr = skeleton <$> fn_type_strs
    let (k, hole_expr) = head $ toList fn_hole_exprs
    -- say $ show [d| k |]
    say $ "task fn: " ++ k
    let fn_types_ = toMapBy kept_fns (fn_types !)
    candidates <- fillHole 0 fn_types_ hole_expr
    say ("candidates: " ++ show (prettyPrint <$> candidates))
