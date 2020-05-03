{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | synthesizer logic
module Synthesis.Synthesizer (module Synthesis.Synthesizer) where

import System.Log.Logger
import GHC.TypeNats (type (+))
import GHC.TypeLits
import Data.Yaml
import Data.HashMap.Lazy (size)
import Control.Monad (void)
import Language.Haskell.Interpreter (Interpreter, liftIO)
import Torch.Internal.Managed.Type.Context (manual_seed_L)
import Torch.Typed.Tensor
import Torch.Typed.Functional
import Torch.Typed.Factories
import Torch.Typed.Aux
import qualified Torch.DType                   as D
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
main = if False -- hasCuda
        then synthesize @Gpu
        else synthesize @Cpu

synthesize :: forall device . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64) => IO ()
synthesize = do
    cfg :: SynthesizerConfig <- parseSynthesizerConfig
    putStrLn $ show cfg
    let SynthesizerConfig{..} = cfg
    updateGlobalLogger logger . setLevel $ logPriority verbosity
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    putStrLn $ show generationCfg
    manual_seed_L $ fromIntegral seed
    (!! length exprBlocks) $ getRules @device @0 cfg taskFnDataset

getRules :: forall device rules . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getRules cfg taskFnDataset = (:)
        ((!! (size (charMap taskFnDataset) + 1)) $ getMaxChar @device @rules @0 cfg taskFnDataset)
        $ getRules @device @(rules + 1) cfg taskFnDataset

getMaxChar :: forall device rules maxChar . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getMaxChar cfg taskFnDataset = (:)
        ((!! (size (dsl taskFnDataset) + natValI @LhsSymbols)) $ getSymbols @device @rules @maxChar @0 cfg taskFnDataset)
        $ getMaxChar @device @rules @(maxChar + 1) cfg taskFnDataset

getSymbols :: forall device rules maxChar symbols . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar, KnownNat symbols) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getSymbols cfg taskFnDataset = (:)
        ((!! longestString taskFnDataset) $ getMaxStringLength @device @rules @maxChar @symbols @0 cfg taskFnDataset)
        $ getSymbols @device @rules @maxChar @(symbols + 1) cfg taskFnDataset

getMaxStringLength :: forall device rules maxChar symbols maxStringLength . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getMaxStringLength cfg taskFnDataset = let SynthesizerConfig{..} = cfg in (:)
        ((!! m) $ getM @device @0 @rules @maxChar @symbols @maxStringLength cfg taskFnDataset)
        $ getMaxStringLength @device @rules @maxChar @symbols @(maxStringLength + 1) cfg taskFnDataset

getM :: forall device m rules maxChar symbols maxStringLength . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat m, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getM cfg taskFnDataset = (:)
        (void . interpretUnsafe $ train @device @m @EncoderBatch @R3nnBatch @symbols @rules @maxStringLength @maxChar cfg taskFnDataset)
        $ getM @device @(m + 1) @rules @maxChar @symbols @maxStringLength cfg taskFnDataset
