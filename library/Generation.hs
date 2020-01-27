{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase, ImpredicativeTypes, RankNTypes, ScopedTypeVariables #-}

-- | generate task functions and sample input/output pairs
module Generation (fnOutputs, fillHoles, fillHole, genFn, genFns, instantiateTypes, instantiateTypeVars, matchesConstraints, matchesType, typeRelation) where

import Language.Haskell.Exts.Syntax (Type(..))
import Language.Haskell.Interpreter (Interpreter, lift, typeChecks, typeChecksWithDetails, typeOf)
import Data.List (nub, delete, minimumBy, isInfixOf, partition)
import Control.Monad (filterM)
import Data.HashMap.Lazy (HashMap, keys, fromList, toList, (!))
import Data.Either (isLeft)
import Data.Set (Set, empty, insert)
import qualified Data.Set
import Types
import FindHoles (findHolesExpr)
import Hint (say, showError, fnIoPairs, exprType)
import Utility (pick, pp, pickKeys, flipOrder)
import Ast (hasHoles, anyFn, numAstNodes)
import Data.Bifunctor (second)
import Util (fstOf3, thdOf3)
import MonadUtils (allM)
import Language.Haskell.Exts.Parser ( ParseResult, parse )
import Data.Ord (Ordering(..))
import Control.Monad (forM, forM_)

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
    partial_ <- filterByCompile partial
    complete_ <- filterByCompile complete
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
filterByCompile :: [(Expr, Set String, Expr)] -> Interpreter [(Expr, Set String, Expr)]
filterByCompile = filterM (fitExpr . thdOf3)
-- unhole . 

-- -- | replace any holes in an expression with undefined, for type-checking purposes
-- unhole :: Expr -> Expr
-- unhole expr = ?

-- TODO: switch to `matchesType`?
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
    if isLeft checks then return False else do
        -- for currying, not for lambdas: use type to filter out non-function programs
        -- say $ "checking if fn type: " ++ pp expr
        -- tp <- exprType expr
        res :: ParseResult Tp <- fmap parse <$> typeOf $ pp expr
        let ok = case unParseResult res of
                Right tp -> case tp of
                    -- say $ "tp: " ++ pp tp
                    TyForall _l _maybeTyVarBinds _maybeContext typ -> case typ of
                        TyFun _l _a _b -> True
                        _ -> False
                    TyFun _l _a _b -> True
                    _ -> False
                Left _e -> False
                -- error $ "failed to parse type " ++ s ++ ": " ++ e
        -- say $ "ok: " ++ show ok
        return ok

-- | given sample inputs by type and type instantiations for a function, get its in/out pairs (by type)
fnOutputs :: HashMap String [Expr]
          -> String
          -> [[String]]                             -- ^ for each type instantiation, for each param, the input type as string
          -> Interpreter (HashMap [String] String)
fnOutputs instantiation_inputs fn_str str_in_instantiations =
        fromList . zip str_in_instantiations <$> mapM (fnIoPairs n fn_str) in_strs
        where
            -- a list of samples for parameters for types
            inputs :: [[[Expr]]] = fmap (instantiation_inputs !) <$> str_in_instantiations
            -- tuples of samples by param
            param_combs :: [[[Expr]]] = sequence <$> inputs
            n = length . head . head $ param_combs
            in_strs :: [String] = pp . list . fmap tuple <$> param_combs

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

-- TODO: defined `Ord` on `Tp` then use `compare :: a -> a -> Ordering`?
-- | find how two types relate
typeRelation :: Tp -> Tp -> Interpreter Ordering
typeRelation a b = do
    sub   <- a `matchesType` b
    super <- b `matchesType` a
    return $ if sub then
        if super then EQ else LT
    else
        if super then GT else error "types don't match!"

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
matchesConstraints :: Tp -> [Tp] -> Interpreter Bool
matchesConstraints tp constraints = do
        let a :: Tp = tyVar "a"
        let forAll = tyForall Nothing (Just $ cxTuple $ (\(TyCon _l qname) -> classA qname [a]) <$> constraints) a
        if null constraints then return True else matchesType tp forAll
