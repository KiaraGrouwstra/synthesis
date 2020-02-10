{-# LANGUAGE DeriveGeneric #-}

-- | self-defined types
module Synthesis.Data
  ( L,
    Tp,
    Expr,
    Hole,
    Stuff (..),
  )
where

import Data.HashMap.Lazy (HashMap)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import GHC.Generics (Generic)
import Language.Haskell.Exts.Syntax
  ( Exp (..),
    SpecialCon (..),
    Type (..),
  )

-- these verbose types annoy me so let's alias them
type L = SrcSpanInfo

type Tp = Type L

type Expr = Exp L

-- | deprecated, not in use
type Hole = SpecialCon L -- ExprHole
    -- type Fn = TyFun L (Type L a) (Type L b)

-- | things I wanna transfer between generation and synthesis sessions
data Stuff = Stuff { fn_types :: HashMap Expr Tp
                    , fn_in_type_instance_outputs :: HashMap Expr (HashMap [Tp] String)
                    , fn_in_type_instantiations :: HashMap Expr [[Tp]]
                    , rest_instantiation_inputs :: HashMap Tp [Expr]
                    , datasets :: ([Expr], [Expr], [Expr])
                    } deriving (Show, Generic)
