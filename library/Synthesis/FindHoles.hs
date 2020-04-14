{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | find holes in an AST
module Synthesis.FindHoles (module Synthesis.FindHoles) where

import Language.Haskell.Exts.Syntax
import Synthesis.Data (Expr)
import Synthesis.Utility

-- | get the first sub-expression
gtrExpr :: Exp l -> Exp l
gtrExpr x = case x of
  Let _l _binds xp -> xp
  App _l xp _exp2 -> xp
  Paren _l xp -> xp
  ExpTypeSig _l xp _tp -> xp
  Var _l _qname -> x
  _ -> error $ "unknown get expr: " <> pp x

-- | set the first sub-expression
strExpr :: Exp l -> Exp l -> Exp l
strExpr x xp = case x of
  Let l binds _exp -> Let l binds xp
  App l _exp1 xp2 -> App l xp xp2
  Paren l _exp -> Paren l xp
  ExpTypeSig l _exp tp -> ExpTypeSig l xp tp
  Var _l _qname -> xp
  _ -> error $ "unknown set expr: " <> pp x

-- | look for holes in an expression. to simplify extracting type,
-- | we will only look for holes as part of an ExpTypeSig, e.g. `_ :: Bool`.
-- | I couldn't figure out how to get this to typecheck as a whole lens,
-- | so instead I'm taking them as getter/setter pairs...
findHolesExpr :: Exp l1 -> [(Exp l2 -> Exp l2, Exp l3 -> Exp l3 -> Exp l3)]
findHolesExpr expr =
  let f = findHolesExpr
      -- by the time we use the lens, we already know exactly how we're navigating.
      -- however, at compile-time this isn't known, so we have to pretend we're still checking here.
      mapLenses (a, b) = (a . gtrExpr, composeSetters strExpr gtrExpr b)
   in case expr of
        Let _l _binds xpr -> mapLenses <$> f xpr
        App _l exp1 exp2 -> (mapLenses <$> f exp1) ++ (mapLenses2 <$> f exp2)
          where
            gtr2 x = case x of (App _l _exp1 xp2) -> xp2; _ -> x
            str2 x xp2 = case x of (App l xp1 _exp2) -> App l xp1 xp2; _ -> x
            mapLenses2 (a, b) = (a . gtr2, composeSetters str2 gtr2 b)
        Paren _l xpr -> mapLenses <$> f xpr
        ExpTypeSig _l xpr _tp -> case xpr of
          Var _l qname -> case qname of
            -- Special _l specialCon -> case specialCon of
            --     ExprHole _l -> [(id, flip const)]
            --     _ -> mapLenses <$> f xpr
            UnQual _l name -> case name of
              Ident _l str -> case str of
                "undefined" -> [(id, flip const)]
                _ -> mapLenses <$> f xpr
              _ -> mapLenses <$> f xpr
            _ -> mapLenses <$> f xpr
          _ -> mapLenses <$> f xpr
        _ -> []
