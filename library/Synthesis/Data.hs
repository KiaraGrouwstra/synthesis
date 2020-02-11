{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | self-defined types
module Synthesis.Data
  ( L,
    Tp,
    Expr,
    Hole,
    Stuff (..),
    GenerationConfig (..),
    SynthesizerConfig (..),
  )
where

import Data.HashMap.Lazy (HashMap, singleton, insert)
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import GHC.Generics (Generic)
import Language.Haskell.Exts.Syntax

-- these verbose types annoy me so let's alias them
type L = SrcSpanInfo

type Tp = Type L

type Expr = Exp L

-- | deprecated, not in use
type Hole = SpecialCon L -- ExprHole

-- | things I wanna transfer between generation and synthesis sessions
data Stuff = Stuff { fn_types :: HashMap Expr Tp
                    , fn_in_type_instance_outputs :: HashMap Expr (HashMap [Tp] String)
                    , fn_in_type_instantiations :: HashMap Expr [[Tp]]
                    , rest_instantiation_inputs :: HashMap Tp [Expr]
                    , datasets :: ([Expr], [Expr], [Expr])
                    } deriving (Show, Generic)

data GenerationConfig = GenerationConfig
  { filePath :: String
  , crashOnError :: Bool
  , seed :: Int
  -- type generation
  , nestLimit :: Int
  , maxInstances :: Int
  -- sample generation
  , numInputs :: Int
  , numMin :: Integer
  , numMax :: Integer
  , listMin :: Int
  , listMax :: Int
  -- function generation
  , maxWildcardDepth :: Int
  , genMaxHoles :: Int
  -- dataset generation
  , training :: Double
  , validation :: Double
  , test :: Double
  } deriving (Show, Generic)

data SynthesizerConfig = SynthesizerConfig
  { filePath :: String
  } deriving (Show, Generic)
