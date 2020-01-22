{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, DataKinds #-}

-- | main logic
module Program (main) where

import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, typeOf, lift)
import Data.List (nub)
import Control.Monad (forM_, forM)
import Hint (runInterpreterMain, say, genInputs, fnIoPairs)
import Ast (fnOutputs, filterTypeSigIoFns, genFns, letRes)
import Types (Tp, Expr, fnTypeIO, instantiateTypes, genTypes, tyCon)
import Utility (groupByVal, flatten, pp, pickKeys)
import Config (nestLimit, maxInstances, numInputs, genMaxHoles)
import FindHoles (gtrExpr)
import Data.HashMap.Lazy (HashMap, empty, insert, elems, (!), mapWithKey, fromList, toList, union)
-- import Debug.Dump (d)

-- | main function, run program in our interpreter monad
main :: IO ()
main = runInterpreterMain program

-- warning: we *must* alias existing functions, or their definitions will be regarded as recursive!
-- | functions used for testing
fnBodies :: HashMap String String
fnBodies = insert "not__" "\\b -> not b" $
-- fnBodies = insert "not_" "\\b -> not b :: Bool -> Bool" $
-- fnBodies = insert "not_" "\\(b :: Bool) -> not b" $
-- fnBodies = insert "not_" "\\b -> let _b = (b :: Bool) in not b" $
-- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
                insert "not_" "not" $
                -- insert "(.)" "(.)" $ -- TODO: combinators error, cannot generate input samples of type function
                insert "id_" "id"  -- TODO: only allow curried version of this function -- applying makes it redundant
                empty

constants :: HashMap String Tp
constants = insert "True" (tyCon "Bool")
            empty

-- | run our program in the interpreter
program :: Interpreter ()
program = do
    let block_asts :: HashMap String Expr = (\str -> fromParseResult (parse str :: ParseResult Expr)) <$> fnBodies
    block_type_strs :: HashMap String String <- mapM typeOf fnBodies
    let block_types :: HashMap String Tp = (\type_str -> fromParseResult (parse type_str :: ParseResult Tp)) <$> block_type_strs

    let blocks = block_types `union` constants
    say "\ngenerating task functions:"
    -- task_fn <- genFn blocks
    -- say $ "task: " ++ show (pp task_fn)
    programs :: [Expr] <- genFns genMaxHoles blocks block_asts
    let task_fns = programs
    forM_ task_fns $ \task_fn -> -- do
        say $ pp $ gtrExpr task_fn

    let fn_strs = pp <$> task_fns
    let fn_asts :: HashMap String Expr = fromList $ zip fn_strs task_fns
    let task_bodies :: HashMap String String = fromList $ zip fn_strs fn_strs
    fn_type_strs :: HashMap String String <- mapM typeOf task_bodies
    let fn_types :: HashMap String Tp = (\type_str -> fromParseResult (parse type_str :: ParseResult Tp)) <$> fn_type_strs

    fill_types :: [Tp] <- nub . flatten <$> lift (genTypes nestLimit maxInstances)
    let fn_type_instantiations :: HashMap String [Tp] = instantiateTypes fill_types <$> fn_types
    let fn_io_type_instantiations :: HashMap String [(Tp, Tp)] = fmap fnTypeIO <$> fn_type_instantiations
    let fn_in_str_type_instantiations :: HashMap String [String] = fmap (pp . fst) <$> fn_io_type_instantiations

    -- casting keys to string cuz Type isn't hashable 😐
    -- let in_type_instantiations :: HashMap String [Tp] = fromList $ zip str_in_types $ nub . flatten <$> in_type_instantiations_
    -- let type_instantiations :: [Tp] = nub $ concat $ elems in_type_instantiations
    let in_type_instantiations :: [Tp] = nub . fmap fst . concat . elems $ fn_io_type_instantiations
    let str_instantiations :: [String] = pp <$> in_type_instantiations
    -- let str_in_type_instantiations :: HashMap String [String] = fmap pp <$> in_type_instantiations
    -- do sample generation not for each function but for each function input type
    -- casting keys to string cuz Type isn't hashable 😐
    instantiation_inputs :: HashMap String String <- fromList . zip str_instantiations <$> mapM (genInputs numInputs) str_instantiations
    -- TODO: even detect functions identical in function but different in scope of inputs -- the type hashmap layer now prevents such comparison
    fn_in_type_instance_outputs :: HashMap String (HashMap String String) <- sequence $ mapWithKey (fnOutputs task_bodies instantiation_inputs) fn_in_str_type_instantiations
    -- group functions with identical type signatures
    let type_sig_fns :: HashMap String [String] = groupByVal $ toList fn_type_strs
    -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
    let type_sig_io_fns :: HashMap String (HashMap String [String]) = (\ks -> groupByVal $ zip ks $ show . (!) fn_in_type_instance_outputs <$> ks) <$> type_sig_fns
    -- deduplicate functions by identical types + io, keeping the shortest
    say "\ndeduplicating task functions:"
    type_sig_io_fns_filtered :: HashMap String (HashMap String String) <- filterTypeSigIoFns fn_asts type_sig_io_fns
    let kept_fns :: [String] = concat $ elems <$> elems type_sig_io_fns_filtered

    say "\nenumerating function i/o examples:"
    -- say "\nfinding fits!"
    forM_ kept_fns $ \k -> do
        let ast :: Expr = fn_asts ! k
        let fn_type_str :: String = fn_type_strs ! k
        say $ "\n" ++ pp (gtrExpr ast) ++ " :: " ++ fn_type_str
        let instantiations :: [String] = fn_in_str_type_instantiations ! k
        let inst_io_pairs :: HashMap String String = pickKeys instantiations $ fn_in_type_instance_outputs ! k
        -- forM_ instantiations $ \tp -> do
        --     let io_pairs = fn_in_type_instance_outputs ! k ! tp
        --     say $ tp ++ ": " ++ io_pairs
        say $ show inst_io_pairs
        let inst_inputs :: HashMap String String = pickKeys instantiations instantiation_inputs
        candidate_ios :: [HashMap String String] <- forM programs $ forM inst_inputs . fnIoPairs . pp
        let candidates :: [Expr] = fmap (letRes . fst) $ filter (\(_expr, inst_ios) -> inst_io_pairs == inst_ios) $ zip programs candidate_ios
        say $ show (pp <$> candidates)
