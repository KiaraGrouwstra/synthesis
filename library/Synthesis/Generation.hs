{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}

-- | generate task functions and sample input/output pairs
module Synthesis.Generation (module Synthesis.Generation) where

import Control.Monad (join, filterM)
import Data.Bifunctor (second)
import Data.Either (isLeft)
import Data.HashMap.Lazy
  ( (!),
    HashMap,
    empty,
    fromList,
    keys,
    elems,
    lookupDefault,
    toList,
    singleton,
  )
import Data.List (minimumBy, partition, nubBy)
import Data.Ord (Ordering (..))
import Data.Set (Set, insert, delete, notMember, isProperSubsetOf)
import Data.Bifunctor (first)
import qualified Data.Set as Set
import Language.Haskell.Exts.Parser (ParseResult, parse)
import Language.Haskell.Exts.Syntax (Type (..))
import Language.Haskell.Interpreter
  ( Interpreter,
    lift,
    typeChecks,
    typeChecksWithDetails,
    typeOf,
  )
import MonadUtils (allM)
import Synthesis.Ast
import Synthesis.FindHoles
import Synthesis.Hint
import Synthesis.Orphanage ()
import Synthesis.Types
import Synthesis.TypeGen
import Synthesis.Data
import Synthesis.Utility
import Util (nTimes, fstOf3, thdOf3)

-- | just directly generate any functions in batch, and see what types end up coming out.
-- | in this approach, further nodes can impose new constraints on the type variables introduced in earlier nodes.
genFns :: Int -> [(String, Expr)] -> HashMap String Expr -> Interpreter [Expr]
genFns maxHoles expr_blocks block_asts =
  fillHoles maxHoles block_asts Set.empty expr_blocks anyFn

-- | generate potential programs filling any holes in a given expression using some building blocks
fillHoles :: Int -> HashMap String Expr -> Set String -> [(String, Expr)] -> Expr -> Interpreter [Expr]
fillHoles maxHoles block_asts used_blocks expr_blocks expr = do
  (partial, candidates) <- fillHole block_asts used_blocks expr_blocks expr
  rest <- case maxHoles of
    0 -> return []
    _ -> mapM (\(inserted, used, _lets) -> fillHoles (maxHoles - 1) block_asts used expr_blocks inserted) partial
  return $ (thdOf3 <$> candidates) ++ concat rest

-- | filter building blocks to those matching a hole in the (let-in) expression, and get the results Exprs
-- | cf. NSPS: uniformly sample programs from the DSL
-- | TODO: also switch to a sampling approach?
fillHole :: HashMap String Expr -> Set String -> [(String, Expr)] -> Expr -> Interpreter ([(Expr, Set String, Expr)], [(Expr, Set String, Expr)])
fillHole block_asts used_blocks expr_blocks expr = do
  -- TODO: save duplicate effort of finding holes: findHolesExpr, hasHoles
  -- TODO: switch `filterByCompile` to `matchesType`?
  partial_ <- filterByCompile partial
  complete_ <- filterByCompile complete
  return (partial_, complete_)
  where
    -- find a hole
    hole_lenses = findHolesExpr expr
    hole_lens = head hole_lenses
    hole_setter :: Expr -> Expr -> Expr = snd hole_lens
    buildExpr :: (String, Expr) -> (Expr, Set String, Expr) = \(block_name, inserted) ->
      let used :: Set String = insert block_name used_blocks
          defs :: HashMap String Expr = pickKeysSafe (Set.toList used) block_asts
          lets :: Expr = if null defs then inserted else letIn defs inserted
       in (inserted, used, lets)
    inserteds :: [(String, Expr)] = second (hole_setter expr) <$> expr_blocks
    triplets :: [(Expr, Set String, Expr)] = buildExpr <$> inserteds
    (partial, complete) :: ([(Expr, Set String, Expr)], [(Expr, Set String, Expr)]) = partition (hasHoles . fstOf3) triplets

-- | filter candidates by trying them in the interpreter to see if they blow up. using the GHC compiler instead would be better.
filterByCompile :: [(Expr, Set String, Expr)] -> Interpreter [(Expr, Set String, Expr)]
filterByCompile = filterM (fitExpr . thdOf3)

-- | check if a candidate fits into a hole by just type-checking the result through the interpreter.
-- | this approach might not be very sophisticated, but... it's for task generation, I don't care if it's elegant.
fitExpr :: Expr -> Interpreter Bool
fitExpr expr = do
  checks <- typeChecksWithDetails $ pp expr
  -- say $ "check: " ++ pp expr
  -- say $ "fitExpr.typeCheck: " ++ case checks of
  --     Right s -> s
  --     Left errors -> show $ showError <$> errors
  -- if isLeft checks then return False else
  if isLeft checks
    then return False
    else do
      -- for currying, not for lambdas: use type to filter out non-function programs
      -- say $ "checking if fn type: " ++ pp expr
      -- tp <- exprType expr
      res :: ParseResult Tp <- fmap parse <$> typeOf $ pp expr
      let ok = case unParseResult res of
            Right tp -> isFn tp && typeSane tp
            Left _e -> False
      -- error $ "failed to parse type " ++ s ++ ": " ++ e
      -- say $ "ok: " ++ show ok
      return ok

-- | given sample inputs by type and type instantiations for a function, get its in/out pairs (by type)
fnOutputs
  :: Bool
  -> HashMap Tp [Expr]
  -> Expr
  -> [([Tp], Tp)]
  -- | for each type instantiation, for each param, the input type as string
  -> Interpreter (HashMap (Tp, Tp) [(Expr, Either String Expr)])
fnOutputs crash_on_error instantiation_inputs fn_ast tp_instantiations =
  case tp_instantiations of
    [] -> return empty
    _ -> do
      let tp_pairs :: [(Tp, Tp)] = first tplIfMultiple <$> tp_instantiations
      let in_instantiations :: [Tp] = fst <$> tp_pairs
      let in_par_instantiations :: [[Tp]] = fst <$> tp_instantiations
      -- a list of samples for parameters for types
      let inputs :: [[[Expr]]] = fmap (flip (lookupDefault []) instantiation_inputs) <$> in_par_instantiations
      -- tuples of samples by param
      -- `sequence` acts as a cartesian product
      let param_combs :: [[[Expr]]] = sequence <$> inputs
      -- if we weren't able to generate valid parameters, return an empty hashmap
      case head param_combs of
        [] -> return empty
        _ -> do
          let n = length . head . head $ param_combs
          let ins :: [Expr] = list . fmap tuple <$> param_combs
          fmap (fromList . zip tp_pairs) $ mapM (uncurry $ fnIoPairs crash_on_error n fn_ast) $ zip in_instantiations ins

-- TODO: c.f. https://hackage.haskell.org/package/ghc-8.6.5/docs/TcHsSyn.html#v:zonkTcTypeToType

-- | generate any combinations of a polymorphic type filled using a list of monomorphic types
instantiateTypes :: HashMap Int [String] -> HashMap Int [Tp] -> Tp -> Interpreter [Tp]
instantiateTypes types_by_arity tps tp = fmap (fillTypeVars tp) <$> instantiateTypeVars types_by_arity tps (findTypeVars tp)

-- | instantiate type variables
instantiateTypeVars :: HashMap Int [String] -> HashMap Int [Tp] -> HashMap String (Int, [Tp]) -> Interpreter [HashMap String Tp]
instantiateTypeVars types_by_arity instTpsByArity variableConstraints = do
  let tpArity :: HashMap Tp Int = fromList $ concat $ fmap (\(i, strs) -> (,i) . tyCon <$> strs) $ toList types_by_arity
  let keyArities :: HashMap String Int = fst <$> variableConstraints
  let arities :: [Int] = elems keyArities
  let ks :: [String] = keys variableConstraints
  let combs :: [[Tp]] = (\tps -> sequence $ replicate (length ks) tps) $ concat $ elems $ instTpsByArity
  let combs_ :: [[Tp]] = filter (\tps -> ((\k -> lookupDefault 0 k tpArity) <$> tps) == arities) combs
  let maps :: [HashMap String Tp] = fromList . zip ks <$> combs_
  let keysOk :: HashMap String Tp -> Interpreter Bool = allM (\(k, v) -> let (arity, tps) = variableConstraints ! k in matchesConstraints arity v tps) . toList
  res <- filterM keysOk maps
  return res

-- | check if type `a` matches type `b`.
-- | runs a command like `(undefined :: b -> ()) (undefined :: a)`.
-- | without forall `a =>` constraints type variables will always match.
matchesType :: Tp -> Tp -> Interpreter Bool
matchesType a b = do
  -- shift TyForall off the parameter type to the function type
  let (forAll, b_) = case b of
        TyForall _l maybeTyVarBinds maybeContext tp -> (tyForall maybeTyVarBinds maybeContext, tp)
        _ -> (tyForall Nothing Nothing, b)
  let cmd :: String = pp $ app (undef $ forAll $ tyFun b_ unit) $ undef a
  typeChecks cmd

-- | check if a type matches the given type constraints.
-- | runs a command like `(undefined :: (Num a, Eq a) => a -> ()) (undefined :: Bool)`
matchesConstraints :: Int -> Tp -> [Tp] -> Interpreter Bool
matchesConstraints arity tp constraints = do
  let a :: Tp = tyVar "a"
  -- pad with unit type `()` according to arity
  -- TODO: this probably doesn't scale to monads' arity=2 as `()` is kind `*`
  let addArity :: Tp -> Tp = nTimes arity $ \tp' -> tyApp tp' unit
  let tp_ :: Tp = addArity tp
  let a_ :: Tp = addArity a
  let forAll :: Tp = tyForall Nothing (Just $ cxTuple $ (\(TyCon _l qname) -> typeA (unQName qname) a) <$> constraints) a_
  if null constraints then return True else matchesType tp_ forAll

-- | deduplicate functions by matching i/o examples
-- | TODO: dedupe out only functions equivalent to those in validation/test sets, having redundancy within training seems okay
dedupeFunctions :: [Expr] -> HashMap Expr (HashMap (Tp, Tp) [(Expr, Either String Expr)]) -> [Expr]
dedupeFunctions task_fns fn_type_ios = kept_fns
  where
  -- programs by types and i/o
  programsByTypeIOs :: HashMap (HashMap (Tp, Tp) String) [Expr] =
      groupByVal $ toList $ fmap pp_ <$> fn_type_ios
  -- programs that overlap in i/o for any monomorphic parameter combo instantiation
  programsByBehavior :: HashMap (Tp, Tp) (HashMap String [Expr]) =
      fromList $ (\(tpStrMap, exprs) -> second (`singleton` exprs) <.> toList $ tpStrMap) =<< toList programsByTypeIOs
  -- generate any ('ascending') (Expr, Expr) pairs contained in any of those [Expr]; dedupe pairs
  behaviorOverlapping :: [(Expr, Expr)] = nubBy (equating pp_) . join . elems $ ((=<<) createGroups . elems <$> programsByBehavior)
  -- ditch removed expr for any remaining pairs, then continue going over the rest
  kept_fns :: [Expr] = Set.toList . flip (foldr checkExprPair) behaviorOverlapping . Set.fromList $ task_fns
  -- for each pair: ditch the least general or longest or just either
  checkExprPair :: (Expr, Expr) -> Set Expr -> Set Expr =
      \ (e1,e2) exprSet -> if notMember e1 exprSet || notMember e1 exprSet then exprSet else
          let
              hm1 = fn_type_ios ! e1
              hm2 = fn_type_ios ! e2
              ks1 = Set.fromList . keys $ hm1
              ks2 = Set.fromList . keys $ hm2
          in
          -- least general
               if isProperSubsetOf ks1 ks2 then
              if equating pp_ hm1 (pickKeys (keys hm1) hm2)
              then delete e1 exprSet else exprSet
          else if isProperSubsetOf ks2 ks1 then
              if equating pp_ hm2 (pickKeys (keys hm2) hm1)
              then delete e2 exprSet else exprSet
          -- longest
          else case compare (numAstNodes e1) (numAstNodes e2) of
              LT -> delete e2 exprSet
              GT -> delete e1 exprSet
              -- tiebreaker: drop whichever
              -- random should be better but deterministic works without monad
              EQ -> delete e2 exprSet

createGroups :: [a] -> [(a, a)]
createGroups [] = []
createGroups (x:xs) = map ((,) x) xs ++ createGroups xs

-- find the characters occurring in an i/o dataset and hash them to unique contiguous numbers
mkCharMap :: [HashMap (Tp, Tp) [(Expr, Either String Expr)]] -> HashMap Char Int
mkCharMap tps_ios = indexChars $ exprStrs <> tpStrs
    where
    exprStrs :: [String] = (\(i,o) -> pp i <> pp_ o) <$> (join . join $ elems <$> tps_ios)
    tpStrs   :: [String] = (\(i,o) -> pp i <> pp  o) <$> (join        $ keys  <$> tps_ios)

indexChars :: [String] -> HashMap Char Int
indexChars = indexList . Set.toList . flip (foldr Set.insert) "\\\"()" . Set.fromList . join
