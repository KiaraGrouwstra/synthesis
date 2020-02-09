{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | ast manipulation
module Synthesis.Ast
  ( skeleton,
    hasHoles,
    holeExpr,
    numAstNodes,
    letRes,
    genBlockVariants,
    anyFn,
    genUncurry,
    genFnType,
    genFnInType,
    genInputs,
  )
where

import Data.HashMap.Lazy (HashMap, empty, mapWithKey)
import Language.Haskell.Exts.Syntax (Exp (..), Type (..))
import System.Random (randoms, randomRs)
import Synthesis.Configs (maxWildcardDepth, nestLimit, listLengths, intRange)
import Synthesis.FindHoles (findHolesExpr)
import Synthesis.Types
import Synthesis.Utility (generator, pp)
import Util (nTimes)

-- | generate applied variants of a function, e.g. [`id`, `id _`]
genBlockVariants :: HashMap String Tp -> [(String, Expr)]
genBlockVariants block_types =
  let generated :: HashMap String [Expr] = mapWithKey (genHoledVariants maxWildcardDepth) block_types
   in -- under currying we will allow any level of application

      concat $ (\k vs -> (\v -> (k, v)) <$> vs) `mapWithKey` generated

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
          _ -> []
        TyFun _l a b -> genHoledVariants_ maxDepth b $ holed a
        TyWildCard _l _maybeName -> case maxDepth of
          0 -> []
          _ -> genHoledVariants_ (maxDepth - 1) tp $ holed wildcard
        _ -> []

-- | generate a function type, to then generate functions matching this type
-- | deprecated, not in use
genFnType :: IO Tp -- TyFun
genFnType = randomFnType True True nestLimit empty tyVarCount
  where
    tyVarCount :: Int = 0 -- TODO: is this okay?

-- | generate a parameter type, to then generate functions taking this input
-- | deprecated, not in use
genFnInType :: IO Tp -- TyFun
genFnInType = randomType True True nestLimit empty tyVarCount
  where
    tyVarCount :: Int = 0 -- TODO: is this okay?

-- | _ :: (_ -> _)
anyFn :: Expr
anyFn = skeleton $ tyFun wildcard wildcard

-- | get the body of a let-in expression
letRes :: Expr -> Expr
letRes = \case
  (Let _l _binds xp) -> xp
  x -> x

-- | check if an expression contains holes
hasHoles :: Expr -> Bool
hasHoles = not . null . findHolesExpr

-- | number of AST nodes in an Expr
numAstNodes :: Expr -> Int
numAstNodes = foldr (\_node acc -> acc + 1) 0

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
    letters :: [String] = pure <$> ['a' .. nTimes (n -1) succ 'a']

-- | randomly generate a number of samples for a given type
genInputs :: Int -> Tp -> [Expr]
genInputs n tp = nubPp . take n $ exprs
  where
    msg = "cannot generate from unknown type!"
    g = generator
    lengths :: [Int] = randomRs listLengths g
    genList :: Tp -> [Expr] = \typ -> (\n_ -> list $ genInputs n_ typ) <$> lengths
    exprs :: [Expr] = case tp of
      -- these should cover types from randomType
      -- TODO: no nub inside nested genInputs for TyList?
      TyList _l typ -> genList typ
      TyApp _l a b -> case pp a of
        "[]" -> genList b
        _ -> error msg
      TyCon _l qname -> case pp qname of
        "Bool" -> con . show <$> (randoms g :: [Bool])
        "Int" -> int <$> (randomRs intRange g :: [Integer])
        _ -> error msg
      _ -> error msg
