{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | self-defined types
module Synthesis.Data (module Synthesis.Data) where

import Data.HashMap.Lazy (HashMap)
import GHC.Generics (Generic)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)

-- these verbose types annoy me so let's alias them

-- | SrcSpanInfo, stuff I don't care about that `haskell-src-exts` forces upon
-- | me by making it a mandatory (type/actual) parameter to all node types...
type L = SrcSpanInfo

-- | Type node
type Tp = Type L

-- | Expression node, where my branches consist of function application, my leaves of typed holes or variables.
type Expr = Exp L

-- | things I wanna transfer between generation and synthesis sessions
data TaskFnDataset = TaskFnDataset
  { generationCfg :: GenerationConfig
  , dsl :: HashMap String Expr
  , generatedTypes :: HashMap Int [String]  -- i.e. typesByArity
  , fnTypes :: HashMap Expr Tp
  , fnInTypeInstanceOutputs :: HashMap Expr (HashMap [Tp] [(Expr, Either String Expr)])
  , restInstantiationInputs :: HashMap Tp [Expr]
  , datasets :: ([Expr], [Expr], [Expr])
  , exprBlocks :: [(String, Expr)]
  , longestString :: Int
  , charMap :: HashMap Char Int
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
  , seed :: Int
  , numEpochs :: Int
  , modelPath :: String
  -- , encoderBatch :: Int
  -- , r3nnBatch :: Int
  , bestOf :: Int
  , dropoutRate :: Double
  , evalFreq :: Int
  , learningRate :: Float
  , checkWindow :: Int
  , convergenceThreshold :: Float
  , synthMaxHoles :: Int
  } deriving (Show, Generic)

data ViewDatasetConfig = ViewDatasetConfig
  { filePath :: String
  } deriving (Show, Generic)
