{-# LANGUAGE ImpredicativeTypes #-}

-- | ast manipulation
module Synthesis.Ast (module Synthesis.Ast) where

import Data.HashMap.Lazy (HashMap, empty, mapWithKey)
import Language.Haskell.Exts.Syntax (Exp (..), Type (..))
import System.Random (RandomGen, randoms, randomRs)
import Synthesis.FindHoles
import Synthesis.Types
import Synthesis.TypeGen
import Synthesis.Data hiding (nestLimit)
import Synthesis.Utility
import Util (nTimes)

-- | generate applied variants of a function, e.g. [`id`, `id _`]
genBlockVariants :: HashMap String Tp -> [(String, Expr)]
genBlockVariants block_types =
  let generated :: HashMap String [Expr] = mapWithKey genHoledVariants block_types
   in
      -- under currying we will allow any level of application
      concat $ (\k vs -> (\v -> (k, v)) <$> vs) `mapWithKey` generated

-- | as any block/parameter may be a (nested) function, generate variants with holes curried in to get all potential return types
genHoledVariants :: String -> Tp -> [Expr]
genHoledVariants = let
    genHoledVariants' :: Tp -> Expr -> [Expr] = \ tp expr ->
      let holed :: Tp -> Expr = app expr . skeleton
      in expr : case tp of
            TyForall _l maybeTyVarBinds maybeContext typ -> case typ of
              -- TyFun _l a b -> genHoledVariants' b  $ holed a
              -- do I wanna pass along the context to the holes?
              -- ensure type constraints are propagated to the type signatures of variant holes.
              -- that's still simplified since for `max _ _`, `max (undefined :: Ord a => a) (undefined :: Ord a => a)` would be a bit inaccurate, in that the ordinal used in the holes must match, rather than being independent type variables, but bad hole filling there should get picked up by the compiler afterwards -- in which case perhaps this mostly matters if I'm actually using these hole type annotations myself (am I?).
              TyFun _l a b -> genHoledVariants' b' $ holed a'
                where
                    scoped = tyForall maybeTyVarBinds maybeContext
                    a' = scoped a
                    b' = scoped b
              _ -> []
            TyFun _l a b -> genHoledVariants' b $ holed a
            TyWildCard _l _maybeName -> error "unexpected wildcard!"
            _ -> []
  in \ k tp -> genHoledVariants' tp $ var k

-- | _ :: (_ -> _)
anyFn :: Expr
anyFn = skeleton $ tyFun wildcard wildcard

-- | get the body in case of a let-in expression
letRes :: Expr -> Expr
letRes = \case
  Let _l _binds xp -> xp
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
-- replace any holes in an expression with undefined, for type-checking purposes
holeExpr = var "undefined"

-- | make a typed hole for a type
skeleton :: Tp -> Expr
skeleton = expTypeSig holeExpr

-- | generate an expression for an n-ary uncurry function, e.g. for n=2: `\ fn (a, b) -> fn a b`
genUncurry :: Int -> Expr
genUncurry 1 = var "id"
genUncurry n = lambda [pvar fn, ptuple $ pvar <$> letters] $ foldl app (var fn) $ var <$> letters
  where
    fn = "fn"
    letters :: [String] = pure <$> ['a' .. nTimes (n -1) succ 'a']

-- | randomly generate a number of samples for a given type.
-- | this should cover types from `typesByArity`.
-- | cf. NSPS: use a rule-based strategy to compute well-formed input strings that satisfy the pre-conditions of the programs.
genInputs :: RandomGen g => g -> (Integer, Integer) -> (Int, Int) -> Int -> Tp -> [Expr]
genInputs g intRange listLengths n tp = nubPp . take n $ exprs
  where
    f = genInputs g intRange listLengths
    msg = "cannot generate from unknown type!"
    lengths :: [Int] = randomRs listLengths g
    genList :: Tp -> [Expr] = \typ -> (\n_ -> list $ f n_ typ) <$> lengths
    exprs :: [Expr] = case tp of
      TyParen _l a -> f n a
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
