{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, DataKinds, OverloadedStrings #-}

-- | main logic
module Program (main) where

import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, typeOf, lift)
import Data.List (nub)
import Control.Monad (forM_, forM)
import Hint (runInterpreterMain, say, genInputs, fnIoPairs, exprType)
import Ast (letRes, genBlockVariants, filterTypeSigIoFns)
import Generation (fnOutputs, genFns, instantiateTypes)
import Types (Tp, Expr, fnTypeIO, genTypes, tyCon, expTypeSig)
import Utility (groupByVal, flatten, pp, pickKeys, fromKeys, fromVals, mapTuple, randomSplit)
import Configs (nestLimit, maxInstances, numInputs, genMaxHoles, split)
import FindHoles (gtrExpr)
import Data.HashMap.Lazy (HashMap, empty, insert, keys, elems, (!), mapWithKey, fromList, toList, union)
import Blocks (fnAsts, blockAsts, constants)
import Data.Text (strip)
-- import Debug.Dump (d)

-- | main function, run program in our interpreter monad
main :: IO ()
main = runInterpreterMain program

-- | run our program in the interpreter
program :: Interpreter ()
program = do
    say "\ngenerating task functions:"
    block_fn_types :: HashMap String Tp <- mapM exprType fnAsts
    let expr_blocks :: [(String, Expr)] = genBlockVariants block_fn_types ++ toList constants
    programs :: [Expr] <- genFns genMaxHoles expr_blocks blockAsts
    say $ "programs: " ++ show (pp <$> programs)
    let task_fns = programs
    -- forM_ task_fns $ \task_fn ->
    --     say $ pp $ gtrExpr task_fn

    let task_fn_asts :: HashMap String Expr = fromVals pp task_fns
    fn_types :: HashMap String Tp <- mapM exprType task_fn_asts
    say $ "fn_types: " ++ show (pp <$> fn_types)

    fill_types :: [Tp] <- nub . flatten <$> lift (genTypes nestLimit maxInstances)
    say $ "fill_types: " ++ show (pp <$> fill_types)
    fn_type_instantiations :: HashMap String [Tp] <- instantiateTypes fill_types `mapM` fn_types
    -- HashMap String [Tp] :: (Tp -> Interpreter [Tp]) <$> HashMap String Tp
    say $ "fn_type_instantiations: " ++ show (fmap pp <$> fn_type_instantiations)
    let fn_io_type_instantiations :: HashMap String [(Tp, Tp)] = fmap fnTypeIO <$> fn_type_instantiations
    say $ "fn_io_type_instantiations: " ++ show (fmap (mapTuple pp) <$> fn_io_type_instantiations)
    let fn_in_str_type_instantiations :: HashMap String [String] = fmap (pp . fst) <$> fn_io_type_instantiations
    say $ "fn_in_str_type_instantiations: " ++ show fn_in_str_type_instantiations

    -- casting keys to string cuz Type isn't hashable 😐
    -- let in_type_instantiations :: HashMap String [Tp] = fromList $ zip str_in_types $ nub . flatten <$> in_type_instantiations_
    -- let type_instantiations :: [Tp] = nub $ concat $ elems in_type_instantiations
    let in_type_instantiations :: [Tp] = nub . fmap fst . concat . elems $ fn_io_type_instantiations
    let str_instantiations :: [String] = pp <$> in_type_instantiations
    say $ "str_instantiations: " ++ show str_instantiations
    -- let str_in_type_instantiations :: HashMap String [String] = fmap pp <$> in_type_instantiations
    -- do sample generation not for each function but for each function input type
    -- casting keys to string cuz Type isn't hashable 😐
    instantiation_inputs :: HashMap String String <- fromList . zip str_instantiations <$> mapM (genInputs numInputs) str_instantiations
    say $ "instantiation_inputs: " ++ show instantiation_inputs
    -- TODO: even detect functions identical in function but different in scope of inputs -- the type hashmap layer now prevents such comparison
    fn_in_type_instance_outputs :: HashMap String (HashMap String String) <- sequence $ mapWithKey (fnOutputs instantiation_inputs) fn_in_str_type_instantiations
    say $ "fn_in_type_instance_outputs: " ++ show fn_in_type_instance_outputs
    -- group functions with identical type signatures
    let type_sig_fns :: HashMap String [String] = groupByVal $ toList $ pp <$> fn_types
    -- strip . 
    say $ "type_sig_fns: " ++ show type_sig_fns
    -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
    let type_sig_io_fns :: HashMap String (HashMap String [String]) = (\ks -> groupByVal $ zip ks $ show . (!) fn_in_type_instance_outputs <$> ks) <$> type_sig_fns
    say $ "type_sig_io_fns: " ++ show type_sig_io_fns

    -- deduplicate functions by identical types + io, keeping the shortest
    say "\ndeduplicating task functions:"
    let type_sig_io_fns_filtered :: HashMap String (HashMap String String) = filterTypeSigIoFns task_fn_asts type_sig_io_fns
    say $ "type_sig_io_fns_filtered: " ++ show type_sig_io_fns_filtered
    -- TODO: dedupe out only functions equivalent to those in validation/test sets, having redundancy within training seems okay
    let kept_fns :: [String] = concat $ elems <$> elems type_sig_io_fns_filtered
    say $ "kept_fns: " ++ show kept_fns
    -- it's kinda weird this splitting is non-monadic, cuz it should be random
    let (train, validation, test) :: ([String], [String], [String]) = randomSplit split kept_fns
    -- TODO: save/load task function data to separate generation/synthesis
    -- TODO: actually use these sets from a learner

    say "\nenumerating function i/o examples:"
    -- say "\nfinding fits!"
    forM_ kept_fns $ \k -> do
        let ast :: Expr = task_fn_asts ! k
        say $ "ast: " ++ pp ast
        let fn_type :: Tp = fn_types ! k
        say $ "fn_type: " ++ pp fn_type
        say $ "\n" ++ pp (expTypeSig (gtrExpr ast) fn_type)
        let instantiations :: [String] = fn_in_str_type_instantiations ! k
        say $ "instantiations: " ++ show instantiations
        let inst_io_pairs :: HashMap String String = pickKeys instantiations $ fn_in_type_instance_outputs ! k
        say $ "inst_io_pairs: " ++ show inst_io_pairs
        -- forM_ instantiations $ \tp -> do
        --     let io_pairs = fn_in_type_instance_outputs ! k ! tp
        --     say $ tp ++ ": " ++ io_pairs
        -- say $ show inst_io_pairs

        -- -- synthesize matching programs by brute force
        -- let inst_inputs :: HashMap String String = pickKeys instantiations instantiation_inputs
        -- candidate_ios :: [HashMap String String] <- forM programs $ forM inst_inputs . fnIoPairs . pp
        -- let candidates :: [Expr] = fmap (letRes . fst) $ filter (\(_expr, inst_ios) -> inst_io_pairs == inst_ios) $ zip programs candidate_ios
        -- say $ show (pp <$> candidates)
