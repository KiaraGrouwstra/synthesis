{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}

-- | generate task functions and sample input/output pairs
module Synthesis.Generation (module Synthesis.Generation) where

import Control.Monad (filterM)
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
  )
import Data.List (minimumBy, partition)
import Data.Ord (Ordering (..))
import Data.Set (Set, insert)
import qualified Data.Set
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
-- import Synthesis.Configs
import Util (nTimes, fstOf3, thdOf3)

-- | just directly sample a generated function, and see what types end up coming out.
genFn :: Int -> [(String, Expr)] -> HashMap String Expr -> Interpreter Expr
genFn maxHoles expr_blocks block_asts = do
  candidates <- genFns maxHoles expr_blocks block_asts
  lift $ pick candidates

-- genFn = lift . fmap pick . genFns

-- | just directly generate any functions in batch, and see what types end up coming out.
-- | in this approach, further nodes can impose new constraints on the type variables introduced in earlier nodes.
genFns :: Int -> [(String, Expr)] -> HashMap String Expr -> Interpreter [Expr]
genFns maxHoles expr_blocks block_asts =
  fillHoles maxHoles block_asts Data.Set.empty expr_blocks anyFn

-- | generate potential programs filling any holes in a given expression using some building blocks
fillHoles :: Int -> HashMap String Expr -> Set String -> [(String, Expr)] -> Expr -> Interpreter [Expr]
fillHoles maxHoles block_asts used_blocks expr_blocks expr = do
  (partial, candidates) <- fillHole block_asts used_blocks expr_blocks expr
  -- say $ "partial: " <> pp_ partial
  rest <- case maxHoles of
    0 -> return []
    _ -> mapM (\(inserted, used, _lets) -> fillHoles (maxHoles - 1) block_asts used expr_blocks inserted) partial
  return $ (thdOf3 <$> candidates) ++ concat rest

-- | filter building blocks to those matching a hole in the (let-in) expression, and get the results Exprs
-- | cf. NSPS: uniformly sample programs from the DSL
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
    -- TODO: let a learner pick a hole
    hole_lens = head hole_lenses
    hole_setter :: Expr -> Expr -> Expr = snd hole_lens
    buildExpr :: (String, Expr) -> (Expr, Set String, Expr) = \pair ->
      let (block_name, inserted) = pair
          used :: Set String = insert block_name used_blocks
          defs :: HashMap String Expr = pickKeysSafe (Data.Set.toList used) block_asts
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
    -- TODO: add explicit type signature for this i/o so programs won't fail type check over uninferred types
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
fnOutputs ::
  Bool ->
  HashMap Tp [Expr] ->
  Expr ->
  -- | for each type instantiation, for each param, the input type as string
  [[Tp]] ->
  Interpreter (HashMap [Tp] [(Expr, Either String Expr)])
fnOutputs crash_on_error instantiation_inputs fn_ast in_instantiations =
  -- do
  -- say $ "instantiation_inputs: " ++ pp_ instantiation_inputs
  -- say $ "fn_str: " ++ pp fn_ast
  -- say $ "in_instantiations: " ++ pp_ in_instantiations
  case in_instantiations of
    [] -> return empty
    _ -> do
      -- a list of samples for parameters for types
      let inputs :: [[[Expr]]] = fmap (flip (lookupDefault []) instantiation_inputs) <$> in_instantiations
      -- say $ "inputs: " ++ pp_ inputs
      -- tuples of samples by param
      let param_combs :: [[[Expr]]] = sequence <$> inputs
      -- say $ "param_combs: " ++ pp_ param_combs
      -- if we weren't able to generate valid parameters, return an empty hashmap
      case head param_combs of
        [] -> return empty
        _ -> do
          let n = length . head . head $ param_combs
          -- say $ "n: " ++ show n
          let ins :: [Expr] = list . fmap tuple <$> param_combs
          -- say $ "ins: " ++ pp_ ins
          fromList . zip in_instantiations <$> mapM (fnIoPairs crash_on_error n fn_ast) ins

-- TODO: c.f. https://hackage.haskell.org/package/ghc-8.6.5/docs/TcHsSyn.html#v:zonkTcTypeToType

-- | generate any combinations of a polymorphic type filled using a list of concrete types
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
  let combs_ :: [[Tp]] = filter (\tps -> ((tpArity !) <$> tps) == arities) combs
  let maps :: [HashMap String Tp] = fromList . zip ks <$> combs_
  let keysOk :: HashMap String Tp -> Interpreter Bool = allM (\(k, v) -> let (arity, tps) = variableConstraints ! k in matchesConstraints arity v tps) . toList
  res <- filterM keysOk maps
  return res

-- TODO: defined `Ord` on `Tp` then use `compare :: a -> a -> Ordering`?

-- | find how two types relate
-- | deprecated, not in use
typeRelation :: Tp -> Tp -> Interpreter Ordering
typeRelation a b = do
  sub <- a `matchesType` b
  super <- b `matchesType` a
  return $
    if sub
      then if super then EQ else LT
      else if super then GT else error "types don't match!"

-- | check if type `a` matches type `b`.
-- | runs a command like `(undefined :: b -> ()) (undefined :: a)`.
-- | without forall `a =>` constraints type variables will always match.
matchesType :: Tp -> Tp -> Interpreter Bool
matchesType a b = do
  -- let cmd :: String = pp $ app (undef $ tyFun b unit) $ undef a
  -- shift TyForall off the parameter type to the function type
  let (forAll, b_) = case b of
        TyForall _l maybeTyVarBinds maybeContext tp -> (tyForall maybeTyVarBinds maybeContext, tp)
        _ -> (tyForall Nothing Nothing, b)
  let cmd :: String = pp $ app (undef $ forAll $ tyFun b_ unit) $ undef a
  -- say cmd
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
  let forAll :: Tp = tyForall Nothing (Just $ cxTuple $ (\(TyCon _l qname) -> typeA qname a) <$> constraints) a_
  if null constraints then return True else matchesType tp_ forAll

-- | deduplicate functions by identical types + io, keeping the shortest
-- | deprecated, not in use
dedupeFunctions :: HashMap Expr Tp -> HashMap Expr (HashMap [Tp] String) -> [Expr]
dedupeFunctions fn_types fn_in_type_instance_outputs = kept_fns
  where
  -- minByMap :: (Foldable t, Ord b) => (a -> b) -> t a -> a = \fn -> 
  minByMap fn = minimumBy $ \a b -> compare (fn a) (fn b)
  -- group functions with identical type signatures
  type_sig_fns :: HashMap Tp [Expr] = groupByVal $ toList fn_types
  -- group functions with identical type signatures + io examples, i.e. functions that are actually equivalent
  -- for each uninstantiated type signature, a map for each type instantiation to matching expressions, from a map from instantiated parameter types to a string of io-pairs
  type_sig_io_fns :: HashMap Tp (HashMap (HashMap [Tp] String) [Expr]) = (\exprs -> groupByVal $ zip exprs $ (!) fn_in_type_instance_outputs <$> exprs) <$> type_sig_fns
  -- for each uninstantiated type signature, a map for each type instantiation to the shortest matching expression, from a map from instantiated parameter types to a string of io-pairs
  type_sig_io_fns_filtered :: HashMap Tp (HashMap (HashMap [Tp] String) Expr) = fmap (minByMap numAstNodes) <$> type_sig_io_fns
  -- TODO: dedupe out only functions equivalent to those in validation/test sets, having redundancy within training seems okay
  kept_fns :: [Expr] = concat $ elems <$> elems type_sig_io_fns_filtered
