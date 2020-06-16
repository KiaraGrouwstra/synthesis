module Synthesis.Configs (module Synthesis.Configs) where

import Synthesis.Data
import Options.Applicative
import Data.Semigroup ((<>))

generationConfig :: Parser GenerationConfig
generationConfig = GenerationConfig
    <$> strOption
        ( long "taskPath"
        <> short 'f'
        <> value "./results/datasets.yml"
        <> showDefault
        <> help "the file path at which to store generated datasets" )
    <*> switch
        ( long "crashOnError"
        <> short 'c'
        <> help "when specified just crash on error while calculating function outputs. otherwise perform an additional typecheck (slower)." )
    <*> seedOpt
    <*> option auto
        ( long "nestLimit"
        <> value 1
        <> showDefault
        <> help "max number of levels of nesting for generated types. high values make for big logs while debugging..." )
    <*> option auto
        ( long "maxInstances"
        <> value 5
        <> showDefault
        <> help "max number of type instantiations to generate for any type variable. may get less after deduplicating type instances." )
    -- NSPS: for all results, the program tree generation is conditioned on a set of 10 input/output string pairs.
    <*> maxHolesOpt
    <*> option auto
        ( long "numInputs"
        <> value 10
        <> showDefault
        <> help "max number of inputs to generate. may get less after nub filters out duplicates." )
    <*> option auto
        ( long "numMin"
        <> value (-20)
        <> showDefault
        <> help "the minimum value for numbers to generate" )
    <*> option auto
        ( long "numMax"
        <> value 20
        <> showDefault
        <> help "the maximum value for numbers to generate" )
    <*> option auto
        ( long "charMin"
        <> value '0'
        <> showDefault
        <> help "the minimum value for characters to generate" )
    <*> option auto
        ( long "charMax"
        <> value '9'
        <> showDefault
        <> help "the maximum value for characters to generate" )
    <*> option auto
        ( long "listMin"
        <> value 0
        <> showDefault
        <> help "the minimum number of elements to generate for list types" )
    <*> option auto
        ( long "listMax"
        <> value 5
        <> showDefault
        <> help "the maximum number of elements to generate for list types" )
    <*> option auto
        ( long "train"
        <> value 0.35
        <> showDefault
        <> help "how much of our dataset to allocate to the training set" )
    <*> option auto
        ( long "validation"
        <> value 0.35
        <> showDefault
        <> help "how much of our dataset to allocate to the validation set" )
    <*> option auto
        ( long "test"
        <> value 0.3
        <> showDefault
        <> help "how much of our dataset to allocate to the test set" )
    <*> option auto
        ( long "maxDataset"
        <> value 1000
        <> showDefault
        <> help "the maximum number of programs we will consider for use in our dataset (before further filtering)" )

parseGenerationConfig :: IO GenerationConfig
parseGenerationConfig = execParser opts
  where
    opts = info (generationConfig <**> helper)
      ( fullDesc
     <> progDesc "generate a program synthesis dataset and dump it to a file"
     <> header "synthesis dataset generation" )

synthesizerConfig :: Parser SynthesizerConfig
synthesizerConfig = SynthesizerConfig
    <$> taskPathOpt
    <*> seedOpt
    <*> numEpochsOpt
    -- <*> encoderBatchOpt
    -- <*> r3nnBatchOpt
    <*> bestOfOpt
    <*> dropoutRateOpt
    <*> evalFreqOpt
    <*> learningRateOpt
    <*> checkWindowOpt
    <*> convergenceThresholdOpt
    <*> resultFolderOpt
    <*> learningDecayOpt
    <*> regularizationOpt
    <*> verbosityOpt
    <*> mOpt
    <*> hOpt
    <*> synthesizerOpt
    <*> maskBadOpt
    <*> useTypesOpt

parseSynthesizerConfig :: IO SynthesizerConfig
parseSynthesizerConfig = execParser opts
  where
    opts = info (synthesizerConfig <**> helper)
      ( fullDesc
     <> progDesc "train a synthesizer on a dataset"
     <> header "program synthesizer" )

gridSearchConfig :: Parser GridSearchConfig
gridSearchConfig = GridSearchConfig
    <$> taskPathOpt
    <*> seedOpt
    <*> numEpochsOpt
    -- <*> encoderBatchOpt
    -- <*> r3nnBatchOpt
    <*> bestOfOpt
    -- <*> dropoutRateOpt
    <*> evalFreqOpt
    <*> learningRateOpt
    <*> checkWindowOpt
    <*> convergenceThresholdOpt
    <*> resultFolderOpt
    <*> learningDecayOpt
    -- <*> regularizationOpt
    <*> verbosityOpt
    <*> evalRoundsOpt
    <*> maskBadOpt
    <*> useTypesOpt

parseGridSearchConfig :: IO GridSearchConfig
parseGridSearchConfig = execParser opts
  where
    opts = info (gridSearchConfig <**> helper)
      ( fullDesc
     <> progDesc "perform hyperparameter optimization by a naive grid search"
     <> header "grid search" )

evolutionaryConfig :: Parser EvolutionaryConfig
evolutionaryConfig = EvolutionaryConfig
    <$> taskPathOpt
    <*> seedOpt
    <*> numEpochsOpt
    -- <*> encoderBatchOpt
    -- <*> r3nnBatchOpt
    <*> bestOfOpt
    -- <*> dropoutRateOpt
    <*> evalFreqOpt
    <*> learningRateOpt
    <*> checkWindowOpt
    <*> convergenceThresholdOpt
    <*> resultFolderOpt
    <*> learningDecayOpt
    -- <*> regularizationOpt
    <*> verbosityOpt
    -- <*> evalRoundsOpt
    <*> maskBadOpt
    <*> useTypesOpt

parseEvolutionaryConfig :: IO EvolutionaryConfig
parseEvolutionaryConfig = execParser opts
  where
    opts = info (evolutionaryConfig <**> helper)
      ( fullDesc
     <> progDesc "perform hyperparameter optimization by a genetic algorithm"
     <> header "evolutionary" )

viewDatasetConfig :: Parser ViewDatasetConfig
viewDatasetConfig = ViewDatasetConfig
    <$> taskPathOpt

parseViewDatasetConfig :: IO ViewDatasetConfig
parseViewDatasetConfig = execParser opts
  where
    opts = info (viewDatasetConfig <**> helper)
      ( fullDesc
     <> progDesc "test a synthesizer on a dataset"
     <> header "program synthesizer" )

-- shared options

taskPathOpt = strOption
    ( long "taskPath"
    <> short 'f'
    <> value "./results/datasets.yml"
    <> showDefault
    <> help "the file path from which to load generated datasets" )

seedOpt = option auto
    ( long "seed"
    <> value (123 :: Int)
    <> showDefault
    <> help "random seed" )

maxHolesOpt = option auto
    ( long "maxHoles"
    <> value (3 :: Int)
    <> showDefault
    <> help "the maximum number of holes to allow in a generated expression" )

numEpochsOpt = option auto
    ( long "numEpochs"
    <> value (1000 :: Int)
    <> showDefault
    <> help "the maximum number of epochs to train for" )

bestOfOpt = option auto
    ( long "bestOf"
    <> value (100 :: Int)
    <> showDefault
    <> help "Number of functions to sample from the model for each latent function and set of input/output examples that we test on, determining success based on the best from this sample." )

encoderBatchOpt = option auto
    ( long "encoderBatch"
    <> value (8 :: Int)
    <> showDefault
    <> help "the encoder batch size i.e. number of samples to process in one go" )

r3nnBatchOpt = option auto
    ( long "r3nnBatch"
    <> value (8 :: Int)
    <> showDefault
    <> help "the R3NN batch size i.e. number of i/o samples to sample per invocation" )

dropoutRateDef = 0.0    -- drop-out not mentioned in NSPS
dropoutRateOpt = option auto
    ( long "dropoutRate"
    <> value dropoutRateDef
    <> showDefault
    <> help "drop-out rate for the encoder LSTM" )

evalFreqOpt = option auto
    ( long "evalFreq"
    <> value (5 :: Int)
    <> showDefault
    <> help "the number of epochs for which to run on train set before evaluating on the validation set again" )

learningRateDef = 0.001
learningRateOpt = option auto
    ( long "learningRate"
    <> value learningRateDef
    <> showDefault
    <> help "initial learning rate used in ML optimizer" )

checkWindowOpt = option auto
    ( long "checkWindow"
    <> value (1 :: Int)
    <> showDefault
    <> help "the window of evaluations to check over to verify convergence" )

convergenceThresholdOpt = option auto
    ( long "convergenceThreshold"
    <> value 0.0
    <> showDefault
    <> help "the minimum loss increment we consider as indicating convergence" )

resultFolderOpt = strOption
    ( long "resultFolder"
    <> short 'f'
    <> value "results"
    <> showDefault
    <> help "the folder in which to store result files" )

learningDecayOpt = option auto
    ( long "learningDecay"
    <> value (1 :: Int)
    <> showDefault
    <> help "by how much to divide the learning rate when accuracy decreases" )

regularizationDef = 0.0
regularizationOpt = option auto
    ( long "regularization"
    <> value regularizationDef
    <> showDefault
    <> help "L2 weight decay used in the optimizer" )

verbosityOpt = strOption
    ( long "verbosity"
    <> short 'v'
    <> value "warning"
    <> showDefault
    <> completeWith ["debug", "info", "notice", "warning", "error", "critical", "alert", "emergency"]
    <> help "the log level to use" )

mDef = (32 :: Int)
mOpt = option auto
    ( long "m"
    <> value mDef
    <> showDefault
    <> help "number of features for R3NN expansions/symbols. must be an even number for H." )

hDef = (32 :: Int)
hOpt = option auto
    ( long "h"
    <> value hDef
    <> showDefault
    <> help "H is the topmost LSTM hidden dimension." )

synthesizerOpt = strOption
    ( long "synthesizer"
    <> value "nsps"
    <> showDefault
    <> completeWith ["random", "nsps"]
    <> help "the synthesizer to use" )

evalRoundsOpt = option auto
    ( long "evalRounds"
    <> value (maxBound :: Int)
    <> showDefault
    <> help "the maximum number of rounds to evaluate for during hyperparameter optimization. by default all configurations are evaluated." )

maskBadOpt = switch
    ( long "maskBad"
    <> short 'm'
    <> help "when specified, compile any possible hole fill to mask out any predictions for non-compiling expressions (present implementation for this is slow but this could be improved)." )

useTypesOpt = switch
    ( long "useTypes"
    <> short 't'
    <> help "supervise synthesis using rule/hole types as additional features." )
