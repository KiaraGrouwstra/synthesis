{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase, ImpredicativeTypes, RankNTypes, ScopedTypeVariables #-}

-- | ast manipulation
module Ast (skeleton, hasHoles, fnOutputs, filterTypeSigIoFns, fillHoles, fillHole, holeExpr, numAstNodes, genFn, genFns, letRes) where

import Language.Haskell.Exts.Syntax ( Type(..), Exp(..), QName(..), SpecialCon(..) )
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Interpreter (Interpreter, lift, typeChecks, typeOf)
import Data.List (nub, delete, minimumBy, isInfixOf, partition )
import Control.Monad (forM, forM_)
import Data.HashMap.Lazy (HashMap, fromList, (!), elems, mapWithKey)
import Data.Maybe (catMaybes)
import Types
import FindHoles
import Hint
import Utility (pick, pp)
import Config (nestLimit, maxWildcardDepth)

-- | generate potential programs filling any holes in a given expression using some building blocks 
fillHoles :: Int -> HashMap String Tp -> Expr -> Interpreter [Expr]
fillHoles maxHoles block_types expr = do
    (partial, candidates) <- fillHole block_types expr
    rest <- case maxHoles of
                0 -> return []
                _ -> mapM (fillHoles (maxHoles - 1) block_types) partial
    return $ candidates ++ concat rest

-- | filter building blocks to those matching a hole in the (let-in) expression, and get the results Exprs
fillHole :: HashMap String Tp -> Expr -> Interpreter ([Expr], [Expr])
fillHole block_types expr = do
    -- find a hole
    let hole_lenses = findHolesExpr expr
    -- TODO: let a learner pick a hole
    let hole_lens = head hole_lenses
    let hole_setter :: Expr -> Expr -> Expr = snd hole_lens
    let together :: HashMap String Tp = block_types
    let generated :: HashMap String [Expr] = mapWithKey (genHoledVariants maxWildcardDepth) together
    -- under currying we will allow any level of application
    let expr_blocks :: [Expr] = concat $ elems generated
    let inserted = hole_setter expr <$> expr_blocks
    let (partial, complete) = partition hasHoles inserted
    -- TODO: given a type, filter partial programs by type-check
    candidates <- filterCandidatesByCompile complete
    return (partial, candidates)

-- | as any block/parameter may be a (nested) function, generate variants with holes curried in to get all potential return types
genHoledVariants :: Int -> String -> Tp -> [Expr]
genHoledVariants maxDepth k tp = genHoledVariants_ maxDepth tp $ var k

-- | internal helper of `genHoledVariants` used for recursion
genHoledVariants_ :: Int -> Tp -> Expr -> [Expr]
genHoledVariants_ maxDepth tp expr =
    let holed = app expr . expTypeSig holeExpr
    in expr : case tp of
    TyFun _l a b -> genHoledVariants_ maxDepth b $ holed a
    TyWildCard _l _maybeName -> case maxDepth of
                0 -> []
                _ -> genHoledVariants_ (maxDepth - 1) tp $ holed wildcard
    _ -> []

-- | filter candidates by trying them in the interpreter to see if they blow up. using the GHC compiler instead would be better.
filterCandidatesByCompile :: [Expr] -> Interpreter [Expr]
filterCandidatesByCompile exprs = fmap catMaybes $ sequence $ fitExpr <$> exprs

-- TODO: ensure blocks are in some let construction!
-- | check if a candidate fits into a hole by just type-checking the result through the interpreter.
-- | this approach might not be very sophisticated, but... it's for task generation, I don't care if it's elegant.
fitExpr :: Expr -> Interpreter (Maybe Expr)
fitExpr expr = do
    checks <- typeChecks $ pp expr
    ok <- if not checks then return False else do
        -- use typeOf to filter out non-function programs
        tp_str <- typeOf $ pp expr
        let tp = fromParseResult (parse tp_str :: ParseResult Tp)
        return $ case tp of
            TyFun _l _a _b -> True
            _ -> False
    return $ if ok then Just expr else Nothing

-- Interpreter.typeOf top expression
-- for each App node:
--      -- get the type signature by Interpreter.typeOf on the function
--      -- using the expected type for the expression, compare with the function signature to potentially fill in (part of) its type variables
--      for the function's parameter slot, assign the substituted input type for the ExprTypeSig

-- -- | generate a function type, to then generate functions matching this type
-- genFnType :: IO Tp -- TyFun
-- genFnType = randomFnType True True nestLimit [] tyVarCount
--     where tyVarCount :: Int = 0 -- TODO: is this okay?

-- -- | generate a parameter type, to then generate functions taking this input
-- genFnInType :: IO Tp -- TyFun
-- genFnInType = randomType True True nestLimit [] tyVarCount
--     where tyVarCount :: Int = 0 -- TODO: is this okay?

-- | _ :: (_ -> _)
anyFn :: Expr
anyFn = skeleton $ tyFun wildcard wildcard

-- | get the body of a let-in expression
letRes :: Expr -> Expr
letRes = \case
    (Let _l _binds xp) -> xp

-- | just directly generate any functions in batch, and see what types end up coming out.
-- | in this approach, further nodes can impose new constraints on the type variables introduced in earlier nodes.
genFns :: Int -> HashMap String Tp -> HashMap String Expr -> Interpreter [Expr]
genFns maxHoles block_types block_asts =
    fillHoles maxHoles block_types $ letIn block_asts anyFn

-- | just directly sample a generated function, and see what types end up coming out.
genFn :: Int -> HashMap String Tp -> HashMap String Expr -> Interpreter Expr
genFn maxHoles block_types block_asts = do
    -- -- TODO: save duplicate effort of finding holes
    -- while hasHoles (genFn_ fn_types) expr
    candidates <- genFns maxHoles block_types block_asts
    lift $ pick candidates
-- genFn = lift . fmap pick . genFns

-- | check if an expression contains holes
hasHoles :: Expr -> Bool
hasHoles = not . null . findHolesExpr

-- | given sample inputs by type and type instantiations for a function, get its in/out pairs (by type)
fnOutputs :: HashMap String String -> HashMap String String -> String -> [String] -> Interpreter (HashMap String String)
fnOutputs fn_bodies instantiation_inputs k instantiations = let
            fn_str = fn_bodies ! k
            inputs = (!) instantiation_inputs <$> instantiations
        in
            fromList . zip instantiations <$> mapM (fnIoPairs fn_str) inputs

-- | number of AST nodes in an Expr
numAstNodes :: Expr -> Int
numAstNodes = foldr (\ _node acc -> acc + 1) 0

-- | find equivalent functions (by type and then input/output) and keep the shortest ones
filterTypeSigIoFns :: HashMap String Expr -> HashMap String (HashMap String [String]) -> Interpreter (HashMap String (HashMap String String))
filterTypeSigIoFns fn_asts type_sig_io_fns = forM type_sig_io_fns $ mapM $ \fns -> do
    -- case length fns of
    --     1 -> return ()
    --     _ -> say $ "deduping equivalent fns: " ++ show fns
    let minByMap fn = minimumBy $ \ a b -> compare (fn a) (fn b)
    -- TODO: keep not just the function with the fewest number of AST nodes, but the more generic one (most type variable names/occurrences) if possible
    let shortest = minByMap (numAstNodes . (!) fn_asts) fns
    let rest = delete shortest fns
    forM_ rest $ \fn ->
        -- say $ "dropping " ++ fn ++ " for terser equivalent " ++ shortest
        say $ pp (gtrExpr (fn_asts ! fn)) ++ " -> " ++ pp (gtrExpr (fn_asts ! shortest))
    return shortest

-- | hole `_` as an AST Expr
holeExpr :: Expr
holeExpr = Var l $ Special l $ ExprHole l

-- | make a typed hole for a type
skeleton :: Tp -> Expr
skeleton = ExpTypeSig l holeExpr
