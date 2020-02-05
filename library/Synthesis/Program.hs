{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | main logic
module Synthesis.Program
  ( main,
  )
where

import Control.Monad (filterM, forM_)
import Data.Bifunctor (first)
import Data.HashMap.Lazy
  ( (!),
    HashMap,
    elems,
    filterWithKey,
    fromList,
    keys,
    mapWithKey,
    toList,
    union,
    member,
  )
import qualified Data.HashMap.Lazy as HM
import Data.List (minimumBy, partition)
import Language.Haskell.Interpreter (Interpreter, lift)
import Synthesis.Ast
  ( genBlockVariants,
    letRes,
    numAstNodes,
  )
import Synthesis.Blocks (blockAsts, constants, fnAsts)
import Synthesis.Configs
  ( genMaxHoles,
    maxInstances,
    nestLimit,
    numInputs,
    split,
  )
import Synthesis.Generation
  ( fnOutputs,
    genFns,
    instantiateTypes,
    matchesType,
  )
import Synthesis.Hint
  ( exprType,
    genInputs,
    runInterpreterMain,
    say,
  )
import Synthesis.Orphanage ()
import Synthesis.Types
  ( Expr,
    Tp,
    expTypeSig,
    fnInputTypes,
    genTypes,
    isFn,
    nubPp,
    parseExpr,
    typeSane,
  )
import Synthesis.Utility
  ( flatten,
    fromKeys,
    fromKeysM,
    groupByVal,
    pickKeysSafe,
    pp,
    pp_,
    randomSplit,
  )
import Util (secondM)

-- | main function, run program in our interpreter monad
main :: IO ()
main = runInterpreterMain program

-- | run our program in the interpreter
program :: Interpreter ()
program = do
  say "generating task functions:"
  block_fn_types :: HashMap String Tp <- mapM exprType fnAsts
  let expr_blocks :: [(String, Expr)] = genBlockVariants block_fn_types ++ toList (fromKeys parseExpr $ keys constants)
  programs :: [Expr] <- genFns genMaxHoles expr_blocks $ filterWithKey (\k v -> k /= pp v) blockAsts
  say "\nprograms:"
  say $ pp_ programs
  let task_fns = programs
  fn_types_ :: HashMap Expr Tp <- fromKeysM exprType task_fns
  say "\nfn_types_:"
  say $ pp_ fn_types_
  let fn_types :: HashMap Expr Tp = HM.filter typeSane fn_types_
  say "\nfn_types:"
  say $ pp_ fn_types
  let task_types :: [Tp] = elems fn_types
  say "\ntask_types:"
  say $ pp_ task_types
  -- generated types we will use for instantiating type variables
  fill_types :: [Tp] <- nubPp . flatten <$> lift (genTypes nestLimit maxInstances)
  say "\nfill_types:"
  say $ pp_ fill_types
  let fn_input_types :: HashMap Expr [Tp] = fnInputTypes <$> fn_types
  say "\nfn_input_types:"
  say $ pp_ fn_input_types
  let input_types :: [Tp] = nubPp . concat . elems $ fn_input_types
  say "\ninput_types:"
  say $ pp_ input_types
  -- split the input types for our programs into functions vs other -- then instantiate others.
  let fns_rest :: ([Tp], [Tp]) = partition isFn input_types
  let mapRest :: [Tp] -> Interpreter [Tp] = fmap concat . mapM (instantiateTypes fill_types)
  (param_fn_types, rest_type_instantiations) :: ([Tp], [Tp]) <- secondM (fmap nubPp . mapRest) $ first nubPp fns_rest
  say "\nparam_fn_types:"
  say $ pp_ param_fn_types
  say "\nrest_type_instantiations:"
  say $ pp_ rest_type_instantiations
  task_instantiations :: [[Tp]] <- instantiateTypes fill_types `mapM` task_types
  -- for each function type, a list of type instantiations
  let type_fn_instantiations :: HashMap Tp [Tp] = fromList $ zip task_types task_instantiations
  say "\ntype_fn_instantiations:"
  say $ pp_ type_fn_instantiations
  let type_in_type_instantiations :: HashMap Tp [[Tp]] = fmap fnInputTypes <$> type_fn_instantiations
  say "\ntype_in_type_instantiations:"
  say $ pp_ type_in_type_instantiations
  let in_type_instantiations :: [Tp] = nubPp . concat . concat . elems $ type_in_type_instantiations
  say "\nin_type_instantiations:"
  say $ pp_ in_type_instantiations
  -- for each function, for each type instantiation, for each param, the input type as string
  let fn_in_type_instantiations :: HashMap Expr [[Tp]] = (type_in_type_instantiations !) <$> fn_types
  say "\nfn_in_type_instantiations:"
  say $ pp_ fn_in_type_instantiations
  -- do sample generation not for each function but for each function input type
  -- for each non-function parameter combo type instantiation, a list of sample expressions
  rest_instantiation_inputs :: HashMap Tp [Expr] <- fromKeysM (genInputs numInputs) rest_type_instantiations
  say "\nrest_instantiation_inputs:"
  say $ pp_ rest_instantiation_inputs
  -- map each parameter function to a filtered map of generated programs matching its type
  let functionMatches :: Tp -> Expr -> Interpreter Bool = \fn_type program_ast -> matchesType (fn_types ! program_ast) fn_type
  let filterFns :: Tp -> Interpreter [Expr] = \fn_type -> filterM (functionMatches fn_type) $ keys fn_types
  -- fn_options :: HashMap Tp [Expr] <- fromKeysM filterFns param_fn_types
  -- say $ "fn_options: " ++ pp_ fn_options
  instantiated_fn_options :: HashMap Tp [Expr] <- fromKeysM filterFns in_type_instantiations
  say "\ninstantiated_fn_options:"
  say $ pp_ instantiated_fn_options
  -- for each parameter combo type instantiation, a list of sample expressions
  let both_instantiation_inputs :: HashMap Tp [Expr] = rest_instantiation_inputs `union` instantiated_fn_options
  say "\nboth_instantiation_inputs:"
  say $ pp_ both_instantiation_inputs
  fn_in_type_instance_outputs :: HashMap Expr (HashMap [Tp] String) <- sequence $ mapWithKey (fnOutputs both_instantiation_inputs) fn_in_type_instantiations
  say "\nfn_in_type_instance_outputs:"
  say $ pp_ fn_in_type_instance_outputs
  -- group functions with identical type signatures
  let type_sig_fns :: HashMap Tp [Expr] = groupByVal $ toList fn_types
  say "\ntype_sig_fns:"
  say $ pp_ type_sig_fns
  -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
  -- for each uninstantiated type signature, a map for each type instantiation to matching expressions, from a map from instantiated parameter types to a string of io-pairs
  let type_sig_io_fns :: HashMap Tp (HashMap (HashMap [Tp] String) [Expr]) = (\exprs -> groupByVal $ zip exprs $ (!) fn_in_type_instance_outputs <$> exprs) <$> type_sig_fns
  say "\ntype_sig_io_fns:"
  say $ pp_ type_sig_io_fns
  say "\n\ndeduplicating task functions:"
  -- deduplicate functions by identical types + io, keeping the shortest
  -- for each uninstantiated type signature, a map for each type instantiation to the shortest matching expression, from a map from instantiated parameter types to a string of io-pairs
  let type_sig_io_fns_filtered :: HashMap Tp (HashMap (HashMap [Tp] String) Expr) = fmap (minByMap numAstNodes) <$> type_sig_io_fns
        where
          minByMap fn = minimumBy $ \a b -> compare (fn a) (fn b)
  say "\ntype_sig_io_fns_filtered:"
  say $ pp_ type_sig_io_fns_filtered
  -- TODO: dedupe out only functions equivalent to those in validation/test sets, having redundancy within training seems okay

  let kept_fns :: [Expr] = concat $ elems <$> elems type_sig_io_fns_filtered
  say "\nkept_fns:"
  say $ pp_ kept_fns
  -- it's kinda weird this splitting is non-monadic, cuz it should be random
  let (_train, _validation, _test) :: ([Expr], [Expr], [Expr]) = randomSplit split kept_fns
  -- TODO: save/load task function data to separate generation/synthesis
  -- TODO: actually use these sets from a learner

  say "\n\nenumerating function i/o examples:"
  -- say "\n\nfinding fits!"
  forM_ kept_fns $ \ast -> do
    let fn_type :: Tp = fn_types ! ast
    say "================================================"
    say $ "\n" ++ pp_ (expTypeSig (letRes ast) fn_type)
    let in_type_instance_outputs :: HashMap [Tp] String = fn_in_type_instance_outputs ! ast
    say "\nin_type_instance_outputs:"
    say $ pp_ in_type_instance_outputs
    let instantiations :: [[Tp]] = fn_in_type_instantiations ! ast
    say "\ninstantiations:"
    say $ pp_ instantiations
    let inst_io_pairs :: HashMap [Tp] String = pickKeysSafe instantiations in_type_instance_outputs
    say "\ninst_io_pairs:"
    say $ pp_ inst_io_pairs
-- -- synthesize matching programs by brute force
-- let inst_inputs :: HashMap String String = pickKeys instantiations rest_instantiation_inputs
-- candidate_ios :: [HashMap String String] <- forM programs $ forM inst_inputs . fnIoPairs . pp
-- let candidates :: [Expr] = fmap (letRes . fst) $ filter (\(_expr, inst_ios) -> PP inst_io_pairs == PP inst_ios) $ zip programs candidate_ios
-- say $ pp_ candidates
