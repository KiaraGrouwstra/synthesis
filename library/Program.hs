{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, DataKinds, OverloadedStrings #-}

-- | main logic
module Program (main) where

import Language.Haskell.Interpreter (Interpreter, lift)
import Data.List (partition, minimumBy)
import Control.Monad (forM_, filterM)
import Hint (runInterpreterMain, say, genInputs, exprType)
import Ast (letRes, genBlockVariants, numAstNodes)
import Generation (fnOutputs, genFns, instantiateTypes, matchesType)
import Types (Tp, Expr, genTypes, expTypeSig, parseExpr, fnInputTypes, isFn, nubTypes)
import Utility (groupByVal, flatten, pp, pickKeys, pickKeysSafe, fromKeys, fromKeysM, randomSplit, ppMap, mapKeys)
import Configs (nestLimit, maxInstances, numInputs, genMaxHoles, split)
import Data.HashMap.Lazy (HashMap, keys, elems, (!), mapWithKey, fromList, toList, union)
import Blocks (fnAsts, blockAsts, constants)
-- import Debug.Dump (d)
import Data.Bifunctor (first)
import Util (secondM)
import Orphanage ()

-- | main function, run program in our interpreter monad
main :: IO ()
main = runInterpreterMain program

-- | run our program in the interpreter
program :: Interpreter ()
program = do
    say "\ngenerating task functions:"
    block_fn_types :: HashMap String Tp <- mapM exprType fnAsts
    let expr_blocks :: [(String, Expr)] = genBlockVariants block_fn_types ++ toList (fromKeys parseExpr $ keys constants)
    programs :: [Expr] <- genFns genMaxHoles expr_blocks blockAsts
    say $ "programs: " ++ show (pp <$> programs)
    let task_fns = programs
    -- forM_ task_fns $ \task_fn ->
    --     say $ pp $ letRes task_fn

    fn_types :: HashMap Expr Tp <- fromKeysM exprType task_fns
    say $ "fn_types: " ++ ppMap (pp <$> fn_types)

    let task_types :: [Tp] = elems fn_types
    say $ "task_types: " ++ show (pp <$> task_types)

    -- generated types we will use for instantiating type variables
    fill_types :: [Tp] <- nubTypes . flatten <$> lift (genTypes nestLimit maxInstances)
    say $ "fill_types: " ++ show (pp <$> fill_types)

    let fn_input_types :: HashMap Expr [Tp] = fnInputTypes <$> fn_types
    say $ "fn_input_types: " ++ ppMap (fmap pp <$> fn_input_types)

    let input_types :: [Tp] = nubTypes . concat . elems $ fn_input_types
    say $ "input_types: " ++ show (pp <$> input_types)

    -- split the input types for our programs into functions vs other -- then instantiate others.
    let fns_rest :: ([Tp], [Tp]) = partition isFn input_types
    let mapRest :: [Tp] -> Interpreter [Tp] = fmap concat . mapM (instantiateTypes fill_types)
    (param_fn_types, rest_type_instantiations) :: ([Tp], [Tp]) <- secondM (fmap nubTypes . mapRest) $ first nubTypes fns_rest
    say $ "param_fn_types: " ++ show (pp <$> param_fn_types)
    say $ "rest_type_instantiations: " ++ show (pp <$> rest_type_instantiations)

    task_instantiations :: [[Tp]] <- instantiateTypes fill_types `mapM` task_types
    -- for each function type, a list of type instantiations
    let type_fn_instantiations :: HashMap Tp [Tp] = fromList $ zip task_types task_instantiations
    say $ "type_fn_instantiations: " ++ ppMap (fmap pp <$> type_fn_instantiations)

    let type_in_type_instantiations :: HashMap Tp [[Tp]] = fmap fnInputTypes <$> type_fn_instantiations
    say $ "type_in_type_instantiations: " ++ ppMap (fmap (fmap pp) <$> type_in_type_instantiations)

    let in_type_instantiations :: [Tp] = nubTypes . concat . concat . elems $ type_in_type_instantiations
    say $ "in_type_instantiations: " ++ show (pp <$> in_type_instantiations)

    -- for each function, for each type instantiation, for each param, the input type as string
    let fn_in_type_instantiations :: HashMap Expr [[Tp]] = (type_in_type_instantiations !) <$> fn_types
    say $ "fn_in_type_instantiations: " ++ ppMap (fmap (fmap pp) <$> fn_in_type_instantiations)

    -- do sample generation not for each function but for each function input type
    -- for each non-function parameter combo type instantiation, a list of sample expressions
    rest_instantiation_inputs :: HashMap Tp [Expr] <- fromKeysM (genInputs numInputs) rest_type_instantiations
    say $ "rest_instantiation_inputs: " ++ ppMap (fmap pp <$> rest_instantiation_inputs)

    -- map each parameter function to a filtered map of generated programs matching its type
    let functionMatches :: Tp -> Expr -> Interpreter Bool = \ fn_type program_ast -> matchesType (fn_types ! program_ast) fn_type
    let filterFns :: Tp -> Interpreter [Expr] = \fn_type -> filterM (functionMatches fn_type) task_fns
    -- fn_options :: HashMap Tp [Expr] <- fromKeysM filterFns param_fn_types
    -- say $ "fn_options: " ++ ppMap (fmap pp <$> fn_options)
    instantiated_fn_options :: HashMap Tp [Expr] <- fromKeysM filterFns in_type_instantiations
    say $ "instantiated_fn_options: " ++ ppMap (fmap pp <$> instantiated_fn_options)

    -- for each parameter combo type instantiation, a list of sample expressions
    let both_instantiation_inputs :: HashMap Tp [Expr] = rest_instantiation_inputs `union` instantiated_fn_options
    say $ "both_instantiation_inputs: " ++ ppMap (fmap pp <$> both_instantiation_inputs)

    fn_in_type_instance_outputs :: HashMap Expr (HashMap [Tp] String) <- sequence $ mapWithKey (fnOutputs both_instantiation_inputs) fn_in_type_instantiations
    say $ "fn_in_type_instance_outputs: " ++ ppMap (mapKeys (fmap pp) <$> fn_in_type_instance_outputs)

    -- group functions with identical type signatures
    let type_sig_fns :: HashMap Tp [Expr] = groupByVal $ toList fn_types
    say $ "type_sig_fns: " ++ ppMap (fmap pp <$> type_sig_fns)

    -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
    -- for each uninstantiated type signature, a map for each type instantiation to matching expressions, from a map from instantiated parameter types to a string of io-pairs
    let type_sig_io_fns :: HashMap Tp (HashMap (HashMap [Tp] String) [Expr]) = (\exprs -> groupByVal $ zip exprs $ (!) fn_in_type_instance_outputs <$> exprs) <$> type_sig_fns
    say $ "type_sig_io_fns: " ++ ppMap (mapKeys (mapKeys (fmap pp)) . fmap (fmap pp) <$> type_sig_io_fns)

    say "\ndeduplicating task functions:"
    -- deduplicate functions by identical types + io, keeping the shortest
    -- for each uninstantiated type signature, a map for each type instantiation to the shortest matching expression, from a map from instantiated parameter types to a string of io-pairs
    let type_sig_io_fns_filtered :: HashMap Tp (HashMap (HashMap [Tp] String) Expr) = fmap (minByMap numAstNodes) <$> type_sig_io_fns
            where minByMap fn = minimumBy $ \ a b -> compare (fn a) (fn b)
    say $ "type_sig_io_fns_filtered: " ++ ppMap (mapKeys (mapKeys (fmap pp)) . fmap pp <$> type_sig_io_fns_filtered)
    -- TODO: dedupe out only functions equivalent to those in validation/test sets, having redundancy within training seems okay

    let kept_fns :: [Expr] = concat $ elems <$> elems type_sig_io_fns_filtered
    say $ "kept_fns: " ++ show (pp <$> kept_fns)

    -- it's kinda weird this splitting is non-monadic, cuz it should be random
    let (_train, _validation, _test) :: ([Expr], [Expr], [Expr]) = randomSplit split kept_fns
    -- TODO: save/load task function data to separate generation/synthesis
    -- TODO: actually use these sets from a learner

    say "\nenumerating function i/o examples:"
    -- say "\nfinding fits!"
    forM_ kept_fns $ \ast -> do
        let fn_type :: Tp = fn_types ! ast
        say $ "\n" ++ pp (expTypeSig (letRes ast) fn_type)
        let instantiations :: [[Tp]] = fn_in_type_instantiations ! ast
        say $ "instantiations: " ++ show (fmap pp <$> instantiations)
        let inst_io_pairs :: HashMap [Tp] String = pickKeysSafe instantiations $ fn_in_type_instance_outputs ! ast
        say $ "inst_io_pairs: " ++ show (mapKeys (fmap pp) inst_io_pairs)
        -- forM_ instantiations $ \tp -> do
        --     let io_pairs = fn_in_type_instance_outputs ! k ! tp
        --     say $ tp ++ ": " ++ io_pairs
        -- say $ show inst_io_pairs

        -- -- synthesize matching programs by brute force
        -- let inst_inputs :: HashMap String String = pickKeys instantiations rest_instantiation_inputs
        -- candidate_ios :: [HashMap String String] <- forM programs $ forM inst_inputs . fnIoPairs . pp
        -- let candidates :: [Expr] = fmap (letRes . fst) $ filter (\(_expr, inst_ios) -> PP inst_io_pairs == PP inst_ios) $ zip programs candidate_ios
        -- say $ show (pp <$> candidates)
