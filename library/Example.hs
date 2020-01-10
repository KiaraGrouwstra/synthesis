{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, DataKinds #-}

-- | main logic
module Example (main) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.Syntax ( Type(TyFun) )
-- import Test.QuickCheck ( Gen, arbitrary, sample, sample', variant, generate, resize )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, interpret, as, typeOf, runInterpreter, lift, setImports)
import Data.List (nub, delete, minimumBy)
import Control.Monad (forM, forM_)
import Utility (Tp, say, errorString, interpretIO, flatten, groupByVal, fnTypeIO, genTypes, instantiateTypes)
import Data.HashMap.Lazy (HashMap, empty, insert, elems, (!), mapWithKey, fromList, toList)
import Data.Ord (compare)
import Debug.Dump (d)

-- | An example function.
main :: IO ()
main = do
    r <- runInterpreter testHint
    case r of
        Left err -> putStrLn $ errorString err
        Right () -> return ()

-- TODO: polyary functions
testHint :: Interpreter ()
testHint = do
    let modules = ["Prelude", "Data.List", "Test.QuickCheck"]
    setImports modules

    -- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
    let fn_bodies = insert "not" "\\b -> let _b = (b :: Bool) in not b" $
                    insert "not_" "not" $
                    insert "id" "id"
                    empty
    -- say [d| fn_bodies |]
    fn_type_strs :: HashMap String String <- mapM typeOf fn_bodies
    -- let fn_hole_exprs :: HashMap String Expr = skeleton <$> fn_type_strs
    -- let tp_strs = nub $ elems fn_type_strs
    let fn_types :: HashMap String Tp = (\type_str -> fromParseResult (parse type_str :: ParseResult Tp)) <$> fn_type_strs
    -- fn_in_types :: HashMap String Tp <- mapM fnInTp fn_type_strs
    -- let in_types :: [Tp] = nub $ elems fn_in_types
    -- let str_in_types :: [String] = prettyPrint <$> in_types
    -- let fn_str_in_types :: HashMap String String = prettyPrint <$> fn_in_types
    fill_types :: [Tp] <- nub . flatten <$> lift genTypes
    -- in_type_instantiations_ :: [Item Tp] <- lift $ mapM instantiateType in_types
    let fn_type_instantiations :: HashMap String [Tp] = instantiateTypes fill_types <$> fn_types
    -- let fn_str_type_instantiations :: HashMap String [String] = fmap prettyPrint <$> fn_type_instantiations
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
    let fnOutputs :: String -> [String] -> Interpreter (HashMap String String) = \k instantiations -> let
             fn_str = fn_bodies ! k
             inputs = (!) instantiation_inputs <$> instantiations
         in
             fromList . zip instantiations <$> mapM (handleInTp fn_str) inputs
    -- TODO: even detect functions identical in function but different in scope of inputs -- the type hashmap layer now prevents such comparison
    fn_in_type_instance_outputs :: HashMap String (HashMap String String) <- sequence $ mapWithKey fnOutputs fn_in_str_type_instantiations
    -- group functions with identical type signatures
    let type_sig_fns :: HashMap String [String] = groupByVal $ toList fn_type_strs
    -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
    let type_sig_io_fns :: HashMap String (HashMap String [String]) = (\ks -> groupByVal $ zip ks $ show . (!) fn_in_type_instance_outputs <$> ks) <$> type_sig_fns
    -- deduplicate functions by identical types + io, keeping the shortest
    type_sig_io_fns_filtered :: HashMap String (HashMap String String) <- forM type_sig_io_fns $ mapM $ \fns -> do
        case length fns of
            1 -> return ()
            _ -> say $ "comparing equivalent fns " ++ show fns
        let minByMap fn = minimumBy $ \ a b -> compare (fn a) (fn b)
        -- TODO: compare by AST nodes, not fn body string length!
        let shortest = minByMap (length . (!) fn_bodies) fns
        let rest = delete shortest fns
        forM_ rest $ \fn ->
            say $ "dropping " ++ fn ++ " for terser equivalent " ++ shortest
        return shortest
    let kept_fns :: [String] = concat $ elems <$> elems type_sig_io_fns_filtered

    forM_ kept_fns $ \k -> do
        let fn_type_str = fn_type_strs ! k
        say $ k ++ " :: " ++ fn_type_str
        let instantiations = fn_in_str_type_instantiations ! k
        forM_ instantiations $ \tp -> do
            let io_pairs = fn_in_type_instance_outputs ! k ! tp
            say $ tp ++ ": " ++ io_pairs

-- | get input-output pairs for a function given the inputs (for one concrete input type instantiation)
handleInTp :: String -> String -> Interpreter String
handleInTp fn_str ins = interpret ("show $ zip (" ++ ins ++ ") $ (" ++ fn_str ++ ") <$> (" ++ ins ++ ")") (as :: String)

-- TODO: evaluate function calls from AST i/o from interpreter, or move to module and import to typecheck
-- | generate examples given a concrete type (to and from string to keep a common type, as we need this for the run-time interpreter anyway)
genInputs :: String -> Interpreter String
genInputs in_type_str = interpretIO $ "let n = 10; seed = 0 in show <$> nub <$> sample' (resize n $ variant seed arbitrary :: Gen (" ++ in_type_str ++ "))"
