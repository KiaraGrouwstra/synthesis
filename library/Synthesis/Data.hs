{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | self-defined types
module Synthesis.Data (module Synthesis.Data) where

import Data.HashMap.Lazy (HashMap, union)
import Data.Csv (Header, ToNamedRecord(..), header, namedRecord, (.=))
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

type Tpl2 a = (a,a)

-- | things I wanna transfer between generation and synthesis sessions
data TaskFnDataset = TaskFnDataset
    { generationCfg :: GenerationConfig
    , dsl :: HashMap String Expr
    , generatedTypes :: HashMap Int [String]  -- i.e. typesByArity
    , fnTypes :: HashMap Expr Tp
    , fnTypeIOs :: HashMap Expr (HashMap (Tp, Tp) [(Expr, Either String Expr)])
    , restInstantiationInputs :: HashMap Tp [Expr]
    , datasets :: ([Expr], [Expr], [Expr])
    , exprBlocks :: [(String, Expr)]
    , longestExprString :: Int
    , longestString :: Int
    , exprCharMap :: HashMap Char Int
    , bothCharMap :: HashMap Char Int
    , ruleCharMap :: HashMap Char Int
    } deriving (Show, Generic)

data GenerationConfig = GenerationConfig
    { taskPath :: String
    , crashOnError :: Bool
    , seed :: Int
    -- type generation
    , nestLimit :: Int
    , maxInstances :: Int
    -- function generation
    , maxHoles :: Int
    -- sample generation
    , numInputs :: Int
    , numMin :: Integer
    , numMax :: Integer
    , charMin :: Char
    , charMax :: Char
    , listMin :: Int
    , listMax :: Int
    -- dataset generation
    , training :: Double
    , validation :: Double
    , test :: Double
    , maxDataset :: Int
    } deriving (Eq, Show, Generic)

data SynthesizerConfig = SynthesizerConfig
    { taskPath :: String
    , seed :: Int
    , numEpochs :: Int
    -- , encoderBatch :: Int
    -- , r3nnBatch :: Int
    , bestOf :: Int
    , dropoutRate :: Double
    , evalFreq :: Int
    , learningRate :: Float
    , checkWindow :: Int
    , convergenceThreshold :: Float
    , resultFolder :: String
    , learningDecay :: Int
    , regularization :: Float  -- TODO: use this
    , verbosity :: String
    , m :: Int
    , h :: Int
    , synthesizer :: String
    , maskBad :: Bool
    , useTypes :: Bool
    } deriving (Eq, Show, Generic)

data GridSearchConfig = GridSearchConfig
    { taskPath :: String
    , seed :: Int
    , numEpochs :: Int
    , bestOf :: Int
    -- , dropoutRate :: Double
    , evalFreq :: Int
    -- , learningRate :: Float
    , checkWindow :: Int
    , convergenceThreshold :: Float
    -- , maxHoles :: Int
    , resultFolder :: String
    , learningDecay :: Int
    -- , regularization :: Float
    , verbosity :: String
    , evalRounds :: Int
    , maskBad :: Bool
    , useTypes :: Bool
    } deriving (Eq, Show, Generic)

-- I should probably include the actual GA config here,
-- but without a refactor I can't make their defaults
-- in evolutionaryConfig depend on hparCombs...
data EvolutionaryConfig = EvolutionaryConfig
    { taskPath :: String
    , seed :: Int
    , numEpochs :: Int
    , bestOf :: Int
    -- , dropoutRate :: Double
    , evalFreq :: Int
    -- , learningRate :: Float
    , checkWindow :: Int
    , convergenceThreshold :: Float
    -- , maxHoles :: Int
    , resultFolder :: String
    , learningDecay :: Int
    -- , regularization :: Float
    , verbosity :: String
    -- , evalRounds :: Int
    , maskBad :: Bool
    , useTypes :: Bool
    } deriving (Eq, Show, Generic)

data OptimizationConfig = OptimizationConfig
    { taskPath :: String
    , seed :: Int
    , numEpochs :: Int
    , bestOf :: Int
    -- , dropoutRate :: Double
    , evalFreq :: Int
    -- , learningRate :: Float
    , checkWindow :: Int
    , convergenceThreshold :: Float
    -- , maxHoles :: Int
    , resultFolder :: String
    , learningDecay :: Int
    -- , regularization :: Float
    , verbosity :: String
    -- , evalRounds :: Int
    , maskBad :: Bool
    , useTypes :: Bool
    } deriving (Eq, Show, Generic)

data HparComb = HparComb
    { learningRate :: Float
    , dropoutRate :: Double
    , regularization :: Float
    , m :: Int
    , h :: Int
    } deriving (Eq, Show, Generic, Ord, Read)

data ViewDatasetConfig = ViewDatasetConfig
    { taskPath :: String
    } deriving (Eq, Show, Generic)

-- I don't actually know what the exclamation mark does, but Aeson used that in their examples
data EvalResult = EvalResult
    { epoch        :: !Int
    , epochSeconds :: !Double
    , lossTrain    :: !Float
    , lossValid    :: !Float
    , accValid     :: !Float
    } deriving (Eq, Show, Generic)

instance ToNamedRecord EvalResult where
    toNamedRecord (EvalResult epoch epochSeconds lossTrain lossValid accValid) =
        namedRecord [ "epoch"        .= epoch
                    , "epochSeconds" .= epochSeconds
                    , "lossTrain"    .= lossTrain
                    , "lossValid"    .= lossValid
                    , "accValid"     .= accValid
                    ]

evalResultHeader :: Header = header ["epoch", "epochSeconds", "lossTrain", "lossValid", "accValid"]

instance ToNamedRecord (HparComb, EvalResult) where
    toNamedRecord (HparComb{..}, evalResult) =
        namedRecord [ "learningRate"   .= learningRate
                    , "dropoutRate"    .= dropoutRate
                    , "regularization" .= regularization
                    , "m"              .= m
                    , "h"              .= h
                    ] `union` toNamedRecord evalResult

gridSearchHeader :: Header = header ["dropoutRate", "regularization", "m", "h"] <> evalResultHeader

combineConfig :: OptimizationConfig -> HparComb -> SynthesizerConfig
combineConfig optCfg hparComb = cfg
  where OptimizationConfig{..} = optCfg
        HparComb{..} = hparComb
        cfg = SynthesizerConfig
                { taskPath             = taskPath
                , seed                 = seed
                , numEpochs            = numEpochs
                , bestOf               = bestOf
                , dropoutRate          = dropoutRate
                , evalFreq             = evalFreq
                , learningRate         = learningRate
                , checkWindow          = checkWindow
                , convergenceThreshold = convergenceThreshold
                , resultFolder         = resultFolder
                , learningDecay        = learningDecay
                , regularization       = regularization
                , verbosity            = verbosity
                , m                    = m
                , h                    = h
                , synthesizer          = "nsps"
                , maskBad              = maskBad
                , useTypes             = useTypes
                }

data PreppedDSL = PreppedDSL
    { variants :: [(String, Expr)]
    , variant_sizes :: HashMap String Int
    , task_type_ins :: HashMap Expr (HashMap (Tp, Tp) [Expr])
    , task_io_map :: HashMap Expr [(Expr, Either String Expr)]
    , task_outputs :: HashMap Expr [Either String Expr]
    , symbolIdxs :: HashMap String Int
    , ruleIdxs :: HashMap String Int
    , variantMap :: HashMap String Expr
    , max_holes :: Int
    , dsl' :: HashMap String Expr
    , variantTypes :: [Tp]
    }
