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
-- cf. NSPS:
-- | We define a program t-steps into construction as a partial program tree (PPT)
-- | (see Figure 3 for a visual depiction).
-- | A PPT has two types of nodes:
-- | - leaf (symbol) nodes
-- | - inner non-leaf (rule) nodes
-- | A leaf node represents a symbol, whether non-terminal or terminal.
-- | An inner non-leaf node represents a particular production rule of the DSL,
-- | where the number of children of the non-leaf node is equivalent to
-- | the arity of the RHS of the rule it represents.
-- | A PPT is called a program tree (PT) whenever all the leaves of the tree are terminal symbols.
-- | Such a tree represents a completed program under the DSL and can be executed.
-- | We define an expansion as the valid application of a specific production rule
-- | (e → e op2 e) to a specific non-terminal leaf node within a PPT (leaf with symbol e).
-- | We refer to the specific production rule that an expansion is derived from as the expansion type.
-- | It can be seen that if there exist two leaf nodes (l1 and l2) with the same
-- | symbol then for every expansion specific to l1 there exists an expansion
-- | specific to l2 with the same type.
type Expr = Exp L

-- -- | deprecated, not in use
-- type Hole = SpecialCon L -- ExprHole

-- | things I wanna transfer between generation and synthesis sessions
data TaskFnDataset = TaskFnDataset
  { generationCfg :: GenerationConfig
  , dsl :: HashMap String Expr
  , generatedTypes :: HashMap Int [String]  -- typesByArity
  , fnTypes :: HashMap Expr Tp
  , fnInTypeInstanceOutputs :: HashMap Expr (HashMap [Tp] [(Expr, Either String Expr)])
  -- , fnInTypeInstantiations :: HashMap Expr [[Tp]]
  , restInstantiationInputs :: HashMap Tp [Expr]
  , datasets :: ([Expr], [Expr], [Expr])
  , exprBlocks :: [(String, Expr)]
  , longestString :: Int
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
  -- , batchSize :: Int
  , bestOf :: Int
  , dropoutRate :: Double
  , evalFreq :: Int
  } deriving (Show, Generic)

data ViewDatasetConfig = ViewDatasetConfig
  { filePath :: String
  } deriving (Show, Generic)
