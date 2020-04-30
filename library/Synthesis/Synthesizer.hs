{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

-- | synthesizer logic
module Synthesis.Synthesizer (module Synthesis.Synthesizer) where

import System.Log.Logger
import GHC.TypeNats (type (+))
import Data.Yaml
import Control.Monad (void)
import Language.Haskell.Interpreter (Interpreter, liftIO)
import Torch.Internal.Managed.Type.Context (manual_seed_L)
import Synthesis.Data
import Synthesis.Hint
import Synthesis.Orphanage ()
import Synthesis.Data (TaskFnDataset (..), SynthesizerConfig (..))
import Synthesis.Configs
import Synthesis.Utility
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.NSPS
import Synthesis.Synthesizer.Params

-- | main function
main :: IO ()
main = do
    cfg :: SynthesizerConfig <- parseSynthesizerConfig
    putStrLn $ show cfg
    let SynthesizerConfig{..} = cfg
    updateGlobalLogger logger . setLevel $ logPriority verbosity
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    putStrLn $ show generationCfg
    manual_seed_L $ fromIntegral seed
    void . interpretUnsafe $ if False -- hasCuda
        then trainGpu @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @N_train @N_validation @N_test @MaxChar cfg taskFnDataset
        else trainCpu @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @N_train @N_validation @N_test @MaxChar cfg taskFnDataset
