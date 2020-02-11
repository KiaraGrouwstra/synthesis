{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | generator logic
module Synthesis.Generator
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
  )
import qualified Data.HashMap.Lazy as HM
import Data.List (partition)
import Data.Store (encode, decodeIO)
import qualified Data.ByteString as BS
import System.Random (StdGen, mkStdGen)
import Language.Haskell.Interpreter (Interpreter, lift, liftIO)
import Synthesis.Ast
  ( genBlockVariants,
    letRes,
  )
import Synthesis.Blocks (blockAsts, constants, fnAsts)
import Synthesis.Generation
  ( fnOutputs,
    genFns,
    instantiateTypes,
    matchesType,
    dedupeFunctions,
  )
import Synthesis.Hint
  ( exprType,
    runInterpreterMain,
    say,
  )
import Synthesis.Ast
  ( genInputs,
  )
import Synthesis.Orphanage ()
import Synthesis.Types
  ( expTypeSig,
    fnInputTypes,
    genTypes,
    isFn,
    nubPp,
    parseExpr,
    typeSane,
  )
import Synthesis.Data (Expr, Tp, Stuff (..), GenerationConfig (..))
import Synthesis.Configs (parseGenerationConfig)
import Synthesis.Utility
  ( flatten,
    fromKeys,
    fromKeysM,
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
  GenerationConfig
      { filePath = filePath
      , crashOnError = crashOnError
      , seed = seed
      -- type generation
      , nestLimit = nestLimit
      , maxInstances = maxInstances
      -- sample generation
      , numInputs = numInputs
      , numMin = numMin
      , numMax = numMax
      , listMin = listMin
      , listMax = listMax
      -- function generation
      , maxWildcardDepth = maxWildcardDepth
      , genMaxHoles = genMaxHoles
      -- dataset generation
      , training = training
      , validation = validation
      , test = test
      } :: GenerationConfig <- liftIO parseGenerationConfig
  let gen :: StdGen = mkStdGen seed
  let split = (training, validation, test)
  say "generating task functions:"
  block_fn_types :: HashMap String Tp <- mapM exprType fnAsts
  let expr_blocks :: [(String, Expr)] = genBlockVariants maxWildcardDepth block_fn_types ++ toList (fromKeys parseExpr $ keys constants)
  programs :: [Expr] <- genFns genMaxHoles expr_blocks $ filterWithKey (\k v -> k /= pp v) blockAsts
  say "\nprograms:"
  say $ pp_ programs
  let task_fns = programs
  fn_types :: HashMap Expr Tp <- fromKeysM exprType task_fns
  say "\nfn_types:"
  say $ pp_ fn_types
  let task_types :: [Tp] = elems fn_types
  say "\ntask_types:"
  say $ pp_ task_types
  -- generated types we will use for instantiating type variables
  fill_types :: HashMap Int [Tp] <- liftIO $ genTypes nestLimit maxInstances
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
  let rest_instantiation_inputs :: HashMap Tp [Expr] = fromKeys (genInputs gen (numMin, numMax) (listMin, listMax) numInputs) rest_type_instantiations
  say "\nrest_instantiation_inputs:"
  say $ pp_ rest_instantiation_inputs
  -- map each parameter function to a filtered map of generated programs matching its type
  let functionMatches :: Tp -> Expr -> Interpreter Bool = \fn_type program_ast -> matchesType (fn_types ! program_ast) fn_type
  let filterFns :: Tp -> Interpreter [Expr] = \fn_type -> filterM (functionMatches fn_type) programs
  -- fn_options :: HashMap Tp [Expr] <- fromKeysM filterFns param_fn_types
  -- say $ "fn_options: " ++ pp_ fn_options
  instantiated_fn_options :: HashMap Tp [Expr] <- fromKeysM filterFns in_type_instantiations
  say "\ninstantiated_fn_options:"
  say $ pp_ instantiated_fn_options
  -- for each parameter combo type instantiation, a list of sample expressions
  let both_instantiation_inputs :: HashMap Tp [Expr] = rest_instantiation_inputs `union` instantiated_fn_options
  say "\nboth_instantiation_inputs:"
  say $ pp_ both_instantiation_inputs
  fn_in_type_instance_outputs :: HashMap Expr (HashMap [Tp] String) <- sequence $ mapWithKey (fnOutputs crashOnError both_instantiation_inputs) fn_in_type_instantiations
  say "\nfn_in_type_instance_outputs:"
  say $ pp_ fn_in_type_instance_outputs

  -- group functions with identical type signatures
  -- let kept_fns :: [Expr] = dedupeFunctions fn_types fn_in_type_instance_outputs
  let kept_fns :: [Expr] = programs
  say "\nkept_fns:"
  say $ pp_ kept_fns

  -- it's kinda weird this splitting is non-monadic, cuz it should be random
  let (train, validation, test) :: ([Expr], [Expr], [Expr]) = randomSplit gen split kept_fns
  -- TODO: save/load task function data to separate generation/synthesis
  -- let stuff = (fn_types, fn_in_type_instance_outputs, fn_in_type_instantiations, rest_instantiation_inputs, (train, validation, test))
  let stuff = Stuff {fn_types=fn_types, fn_in_type_instance_outputs=fn_in_type_instance_outputs, fn_in_type_instantiations=fn_in_type_instantiations, rest_instantiation_inputs=rest_instantiation_inputs, datasets=(train, validation, test)}
  liftIO $ BS.writeFile filePath $ encode stuff

  say "\n\nenumerating function i/o examples:"
  forM_ kept_fns $ \ast -> do
    let fn_type :: Tp = fn_types ! ast
    say "================================================"
    say $ "\n" ++ pp_ (expTypeSig (letRes ast) fn_type)
    let in_type_instance_outputs :: HashMap [Tp] String = fn_in_type_instance_outputs ! ast
    say $ pp_ in_type_instance_outputs
