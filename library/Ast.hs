{-# LANGUAGE TemplateHaskell, QuasiQuotes, LambdaCase, ImpredicativeTypes, RankNTypes, ScopedTypeVariables #-}

-- | ast manipulation
module Ast (skeleton, hasHoles, holeExpr, numAstNodes, letRes, genBlockVariants, anyFn, genUncurry) where
import Language.Haskell.Exts.Syntax ( Type(..), Exp(..) )
import Data.HashMap.Lazy (HashMap, (!), mapWithKey)
import Types
import FindHoles (findHolesExpr)
import Configs (maxWildcardDepth)
import Util (nTimes)

genBlockVariants :: HashMap String Tp -> [(String, Expr)]
genBlockVariants block_types = let
        together :: HashMap String Tp = block_types
        generated :: HashMap String [Expr] = mapWithKey (genHoledVariants maxWildcardDepth) together
        -- under currying we will allow any level of application
    in
        concat $ (\ k vs -> (\v -> (k, v)) <$> vs) `mapWithKey` generated


-- | as any block/parameter may be a (nested) function, generate variants with holes curried in to get all potential return types
genHoledVariants :: Int -> String -> Tp -> [Expr]
genHoledVariants maxDepth k tp = genHoledVariants_ maxDepth tp $ var k

-- | internal helper of `genHoledVariants` used for recursion
genHoledVariants_ :: Int -> Tp -> Expr -> [Expr]
genHoledVariants_ maxDepth tp expr =
    let holed = app expr . expTypeSig holeExpr
    in expr : case tp of
    TyForall _l _maybeTyVarBinds _maybeContext typ -> case typ of
        TyFun _l a b -> genHoledVariants_ maxDepth b $ holed a
    TyFun _l a b -> genHoledVariants_ maxDepth b $ holed a
    TyWildCard _l _maybeName -> case maxDepth of
                0 -> []
                _ -> genHoledVariants_ (maxDepth - 1) tp $ holed wildcard
    _ -> []

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

-- | check if an expression contains holes
hasHoles :: Expr -> Bool
hasHoles = not . null . findHolesExpr

-- | number of AST nodes in an Expr
numAstNodes :: Expr -> Int
numAstNodes = foldr (\ _node acc -> acc + 1) 0

-- | hole `_` as an AST Expr
holeExpr :: Expr
-- holeExpr = var $ Special l $ ExprHole l
-- | replace any holes in an expression with undefined, for type-checking purposes
holeExpr = var "undefined"

-- | make a typed hole for a type
skeleton :: Tp -> Expr
skeleton = ExpTypeSig l holeExpr

-- | generate an expression for an n-ary uncurry function, e.g. for n=2: `\ fn (a, b) -> fn a b`
genUncurry :: Int -> Expr
genUncurry 1 = var "id"
genUncurry n = lambda [pvar fn, ptuple $ pvar <$> letters] $ foldl app (var fn) $ var <$> letters
        where
            fn = "fn"
            letters :: [String] = pure <$> ['a' .. nTimes (n-1) succ 'a']
