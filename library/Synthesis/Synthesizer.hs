{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

-- | synthesizer logic
module Synthesis.Synthesizer (module Synthesis.Synthesizer) where

import GHC.TypeNats (type (+))
import Data.Yaml
import Language.Haskell.Interpreter (Interpreter, liftIO)
import Torch.Internal.Managed.Type.Context (manual_seed_L)
import Synthesis.Hint
import Synthesis.Orphanage ()
import Synthesis.Data (TaskFnDataset (..), SynthesizerConfig (..))
import Synthesis.Configs
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.NSPS
import Synthesis.Synthesizer.Params

-- | main function, run program in our interpreter monad
main :: IO ()
main = interpretUnsafe program

-- | run our program in the interpreter
program :: Interpreter ()
program = do
    cfg :: SynthesizerConfig <- liftIO parseSynthesizerConfig
    say $ show cfg
    let SynthesizerConfig{..} = cfg
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    say $ show generationCfg
    liftIO $ manual_seed_L $ fromIntegral seed
    train @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @N_train @N_validation @N_test @MaxChar cfg taskFnDataset
