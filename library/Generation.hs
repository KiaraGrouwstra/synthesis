{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase, ImpredicativeTypes, RankNTypes, ScopedTypeVariables #-}

-- | generate task functions and sample input/output pairs
module Generation (fnOutputs, fillHoles, fillHole, genFn, genFns, instantiateTypes, instantiateTypeVars, matchesConstraints) where

import Language.Haskell.Exts.Syntax (Type(..))
import Language.Haskell.Interpreter (Interpreter, lift, typeChecks, typeChecksWithDetails)
import Data.List (nub, delete, minimumBy, isInfixOf, partition)
import Control.Monad (filterM)
import Data.HashMap.Lazy (HashMap, keys, fromList, toList, (!))
import Data.Either (isLeft)
import Data.Set (Set, empty, insert)
import qualified Data.Set
import Types
import FindHoles (gtrExpr, findHolesExpr)
import Hint (say, showError, fnIoPairs, exprType)
import Utility (pick, pp, pickKeys, mapTuple)
import Ast (hasHoles, anyFn)
import Data.Bifunctor (second)
import Util (fstOf3, thdOf3)
import MonadUtils (allM)

-- | just directly sample a generated function, and see what types end up coming out.
genFn :: Int -> [(String, Expr)] -> HashMap String Expr -> Interpreter Expr
genFn maxHoles expr_blocks block_asts = do
    -- -- TODO: save duplicate effort of finding holes
    -- while hasHoles (genFn_ fn_types) expr
    candidates <- genFns maxHoles expr_blocks block_asts
    lift $ pick candidates
-- genFn = lift . fmap pick . genFns

-- | just directly generate any functions in batch, and see what types end up coming out.
-- | in this approach, further nodes can impose new constraints on the type variables introduced in earlier nodes.
genFns :: Int -> [(String, Expr)] -> HashMap String Expr -> Interpreter [Expr]
genFns maxHoles expr_blocks block_asts =
    fillHoles maxHoles block_asts empty expr_blocks anyFn

-- | generate potential programs filling any holes in a given expression using some building blocks 
fillHoles :: Int -> HashMap String Expr -> Set String -> [(String, Expr)] -> Expr -> Interpreter [Expr]
fillHoles maxHoles block_asts used_blocks expr_blocks expr = do
    (partial, candidates) <- fillHole block_asts used_blocks expr_blocks expr
    rest <- case maxHoles of
                0 -> return []
                _ -> mapM (\(inserted, used, _lets) -> fillHoles (maxHoles - 1) block_asts used expr_blocks inserted) partial
    return $ (thdOf3 <$> candidates) ++ concat rest

-- | filter building blocks to those matching a hole in the (let-in) expression, and get the results Exprs
fillHole :: HashMap String Expr -> Set String -> [(String, Expr)] -> Expr -> Interpreter ([(Expr, Set String, Expr)], [(Expr, Set String, Expr)])
fillHole block_asts used_blocks expr_blocks expr = do
    partial_ <- filterByCompile False partial
    complete_ <- filterByCompile True complete
    return (partial_, complete_)
    where
        -- find a hole
        hole_lenses = findHolesExpr expr
        -- TODO: let a learner pick a hole
        hole_lens = head hole_lenses
        hole_setter :: Expr -> Expr -> Expr = snd hole_lens
        buildExpr :: (String, Expr) -> (Expr, Set String, Expr) = \pair -> let
                (block_name, inserted) = pair
                used :: Set String = insert block_name used_blocks
                lets :: Expr = letIn (pickKeys (Data.Set.toList used) block_asts) inserted
            in (inserted, used, lets)
        inserteds :: [(String, Expr)] = second (hole_setter expr) <$> expr_blocks
        triplets :: [(Expr, Set String, Expr)] = buildExpr <$> inserteds
        (partial, complete) :: ([(Expr, Set String, Expr)], [(Expr, Set String, Expr)]) = partition (hasHoles . fstOf3) triplets

-- | filter candidates by trying them in the interpreter to see if they blow up. using the GHC compiler instead would be better.
filterByCompile :: Bool -> [(Expr, Set String, Expr)] -> Interpreter [(Expr, Set String, Expr)]
filterByCompile isComplete = filterM (fitExpr isComplete . thdOf3)
-- unhole . 

-- -- | replace any holes in an expression with undefined, for type-checking purposes
-- unhole :: Expr -> Expr
-- unhole expr = ?

-- | check if a candidate fits into a hole by just type-checking the result through the interpreter.
-- | this approach might not be very sophisticated, but... it's for task generation, I don't care if it's elegant.
fitExpr :: Bool -> Expr -> Interpreter Bool
fitExpr checkType expr = do

    checks <- typeChecksWithDetails $ pp expr
    -- say $ "check: " ++ pp expr
    -- say $ "fitExpr.typeCheck: " ++ case checks of
    --     Right s -> s
    --     Left errors -> show $ showError <$> errors
    -- if isLeft checks then return False else
    if isLeft checks then return False else

     if not checkType then return True else do
        -- for currying, not for lambdas: use type to filter out non-function programs
        -- say $ "checking if fn type: " ++ pp expr
        tp <- exprType expr
        -- say $ "tp: " ++ pp tp
        -- say $ "tp: " ++ show tp
        let ok = case tp of
                TyForall _l _maybeTyVarBinds _maybeContext typ -> case typ of
                    TyFun _l _a _b -> True
                    _ -> False
                TyFun _l _a _b -> True
                _ -> False
        -- say $ "ok: " ++ show ok
        return ok

-- | given sample inputs by type and type instantiations for a function, get its in/out pairs (by type)
fnOutputs :: HashMap String String -> String -> [String] -> Interpreter (HashMap String String)
fnOutputs instantiation_inputs fn_str str_in_instantiations =
    let
            inputs :: [String] = (!) instantiation_inputs <$> str_in_instantiations
        in
            fromList . zip str_in_instantiations <$> mapM (fnIoPairs fn_str) inputs

-- TODO: c.f. https://hackage.haskell.org/package/ghc-8.6.5/docs/TcHsSyn.html#v:zonkTcTypeToType
-- | generate any combinations of a polymorphic type filled using a list of concrete types
instantiateTypes :: [Tp] -> Tp -> Interpreter [Tp]
instantiateTypes tps tp = fmap (fillTypeVars tp) <$> instantiateTypeVars tps (findTypeVars tp)

-- | instantiate type variables
instantiateTypeVars :: [Tp] -> HashMap String [Tp] -> Interpreter [HashMap String Tp]
-- instantiateTypeVars tps vars = fromList . zip ks <$> sequence (replicate (length ks) tps)
instantiateTypeVars tps vars = do
    let ks :: [String] = keys vars
    let combs :: [[Tp]] = sequence $ replicate (length ks) tps
    let maps :: [HashMap String Tp] = fromList . zip ks <$> combs
    let keysOk :: HashMap String Tp -> Interpreter Bool = allM (\(k,v) -> matchesConstraints v $ vars ! k) . toList
    filterM keysOk maps

-- | check if a type matches the given type constraints
matchesConstraints :: Tp -> [Tp] -> Interpreter Bool
matchesConstraints tp constraints = do
        let a :: Tp = tyVar "a"
        -- (undefined :: (Num a, Eq a) => a -> a) (undefined :: Bool)
        let cmd :: String = pp $ app (undef (tyForall Nothing (Just $ cxTuple $ (\(TyCon _l qname) -> classA qname [a]) <$> constraints) $ tyFun a a)) $ undef tp

-- -- | find equivalent functions (by type and then input/output) and keep those most generic/short.
-- filterTypeSigIoFnsM :: HashMap String Expr -> HashMap String (HashMap String [String]) -> Interpreter (HashMap String (HashMap String String))
-- filterTypeSigIoFnsM fn_asts type_sig_io_fns = mapM filterFns <$> type_sig_io_fns
--     where
--         -- I'd separate this into a `(a -> a -> Ordering) -> (a -> a -> Ordering) -> (a -> a -> Ordering)` `fallback` function but the monad complicates this...
--         comparator = maximumByM $ \ a b -> do
--             rel <- typeRelation a b
--             return $ case rel of
--                 -- as a tie-break check for smallest AST -- flip for max/min
--                 EQ -> flipOrder $ compare (numNodes a) (numNodes b)
--                 _ -> rel
--             where
--                 numNodes = numAstNodes . (!) fn_asts
--         filterFns fns = do
--                 -- case length fns of
--                 --     1 -> return ()
--                 --     _ -> say $ "deduping equivalent fns: " ++ show fns
--                 -- TODO: keep not just the function with the fewest number of AST nodes, but the more generic one (most type variable names/occurrences) if possible
--                 shortest <- comparator fns
--                 let rest = delete shortest fns
--                 forM_ rest $ \fn ->
--                     -- say $ "dropping " ++ fn ++ " for terser equivalent " ++ shortest
--                     say $ pp (gtrExpr (fn_asts ! fn)) ++ " -> " ++ pp (gtrExpr (fn_asts ! shortest))
--                 return shortest
