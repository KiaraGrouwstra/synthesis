module Synthesis.Configs
  ( module Synthesis.Configs
    -- parseGenerationConfig,
    -- parseSynthesizerConfig,
  )
where

import Synthesis.Data
import Options.Applicative
import Data.Semigroup ((<>))

generationConfig :: Parser GenerationConfig
generationConfig = GenerationConfig
    <$> strOption
        ( long "filePath"
        <> short 'f'
        <> value "./datasets.bin"
        <> showDefault
        <> help "the file path at which to store generated datasets" )
    <*> switch
        ( long "crashOnError"
        <> short 'c'
        <> help "when specified just crash on error while calculating function outputs. otherwise perform an additional typecheck (slower)." )
    <*> option auto
        ( long "seed"
        <> value 123
        <> showDefault
        <> help "random seed" )
    <*> option auto
        ( long "nestLimit"
        <> value 0
        <> showDefault
        <> help "max number of levels of nesting for generated types. high values make for big logs while debugging..." )
    <*> option auto
        ( long "maxInstances"
        <> value 5
        <> showDefault
        <> help "max number of instantiations to generate for any type containing type variables. may get less after deduplicating type instances." )
    -- NSPS: for all results, the program tree generation is conditioned on a set of 10 input/output string pairs.
    <*> option auto
        ( long "numInputs"
        <> value 10
        <> showDefault
        <> help "max number of inputs to generate. may get less after nub filters out duplicates." )
    <*> option auto
        ( long "maxWildcardDepth"
        <> value 2
        <> showDefault
        <> help "the maximum level of functions to imagine in a wildcard for function generation" )
    <*> option auto
        ( long "genMaxHoles"
        <> value 1
        <> showDefault
        <> help "the maximum number of holes to allow in a generated expression" )
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
        ( long "listMin"
        <> value 0
        <> showDefault
        <> help "the minimum number of elements to generate for list types" )
    <*> option auto
        ( long "listMax"
        <> value 5
        <> showDefault
        <> help "the maximum number of elements to generate for list types" )
    -- TODO: figure out a sensible split. NSPS mentions training/test tho not validation.
    -- NSPS: We sample a subset of only 1000 training programs from the 5 million program set to report the training results in the tables. The test sets also consist of 1000 programs.
    <*> option auto
        ( long "train"
        <> value 0.45
        <> showDefault
        <> help "how much of our dataset to allocate to the training set" )
    <*> option auto
        ( long "validation"
        <> value 0.1
        <> showDefault
        <> help "how much of our dataset to allocate to the validation set" )
    <*> option auto
        ( long "test"
        <> value 0.45
        <> showDefault
        <> help "how much of our dataset to allocate to the test set" )

parseGenerationConfig :: IO GenerationConfig
parseGenerationConfig = execParser opts
  where
    opts = info (generationConfig <**> helper)
      ( fullDesc
     <> progDesc "generate a program synthesis dataset and dump it to a file"
     <> header "synthesis dataset generation" )

synthesizerConfig :: Parser SynthesizerConfig
synthesizerConfig = SynthesizerConfig
    <$> strOption
        ( long "filePath"
        <> short 'f'
        <> value "./datasets.bin"
        <> showDefault
        <> help "the file path from which to load generated datasets" )
    <*> option auto
        ( long "seed"
        <> value 123
        <> showDefault
        <> help "random seed" )
    <*> option auto
        ( long "numEpochs"
        <> value 1000
        <> showDefault
        <> help "the number of epochs to train for" )
    <*> strOption
        ( long "modelPath"
        <> short 'm'
        <> value "./synthesis.pt"
        <> showDefault
        <> help "the file path at which to store trained models" )
    -- <*> option auto
    --     ( long "batchSize"
    --     <> value 8
    --     <> showDefault
    --     <> help "the batch size i.e. number of items to process in one go" )
    <*> option auto
        ( long "bestOf"
        <> value 100
        <> showDefault
        <> help "Number of functions to sample from the model for each latent function and set of input/output examples that we test on, determining success based on the best from this sample." )
    <*> option auto
        ( long "dropoutRate"
        <> value 0.0    -- drop-out not mentioned in NSPS
        <> showDefault
        <> help "drop-out rate for the encoder LSTM" )

parseSynthesizerConfig :: IO SynthesizerConfig
parseSynthesizerConfig = execParser opts
  where
    opts = info (synthesizerConfig <**> helper)
      ( fullDesc
     <> progDesc "test a synthesizer on a dataset"
     <> header "program synthesizer" )
