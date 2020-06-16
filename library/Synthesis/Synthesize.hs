{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | synthesizer logic
module Synthesis.Synthesize (module Synthesis.Synthesize) where

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
import Torch.Typed.NN
import Torch.Typed.NN.Recurrent.LSTM
import qualified Torch.DType                   as D
import qualified Torch.NN                      as A
import Synthesis.Data
import Synthesis.Hint
import Synthesis.Orphanage ()
import Synthesis.Data (TaskFnDataset (..), SynthesizerConfig (..))
import Synthesis.Configs
import Synthesis.Utility
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.Encoder
import Synthesis.Synthesizer.TypeEncoder
import Synthesis.Synthesizer.R3NN
import Synthesis.Synthesizer.NSPS
import Synthesis.Synthesizer.Params
import Synthesis.Synthesizer.Random
import Synthesis.Synthesizer.Train

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
    (!! length exprBlocks) $
        -- featMult
        if useTypes then
            getRules @device @2 @0 cfg taskFnDataset
        else
            getRules @device @1 @0 cfg taskFnDataset

getRules :: forall device featMult rules . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getRules cfg taskFnDataset = let
        TaskFnDataset{..} = taskFnDataset
        useTypes = natValI @featMult > 1
        charMap = if useTypes then bothCharMap else exprCharMap
    in (:)
        ((!! (size charMap + 1)) $ getMaxChar @device @featMult @rules @0 cfg taskFnDataset)
        $ getRules @device @featMult @(rules + 1) cfg taskFnDataset

getMaxChar :: forall device featMult rules maxChar . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getMaxChar cfg taskFnDataset = (:)
        ((!! (size (dsl taskFnDataset) + natValI @LhsSymbols)) $ getSymbols @device @featMult @rules @maxChar @0 cfg taskFnDataset)
        $ getMaxChar @device @featMult @rules @(maxChar + 1) cfg taskFnDataset

getSymbols :: forall device featMult rules maxChar symbols . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar, KnownNat symbols) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getSymbols cfg taskFnDataset = let
        useTypes = natValI @featMult > 1
        longest = if useTypes then longestString else longestExprString
    in (:)
        ((!! longest taskFnDataset) $ getMaxStringLength @device @featMult @rules @maxChar @symbols @0 cfg taskFnDataset)
        $ getSymbols @device @featMult @rules @maxChar @(symbols + 1) cfg taskFnDataset

getMaxStringLength :: forall device featMult rules maxChar symbols maxStringLength . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getMaxStringLength cfg taskFnDataset = let SynthesizerConfig{..} = cfg in (:)
        ((!! h) $ getH @device @featMult @rules @maxChar @symbols @maxStringLength @0 cfg taskFnDataset)
        $ getMaxStringLength @device @featMult @rules @maxChar @symbols @(maxStringLength + 1) cfg taskFnDataset

getH :: forall device featMult rules maxChar symbols maxStringLength h . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getH cfg taskFnDataset = let SynthesizerConfig{..} = cfg in (:)
        ((!! m) $ getM @device @featMult @0 @rules @maxChar @symbols @maxStringLength @h cfg taskFnDataset)
        $ getH @device @featMult @rules @maxChar @symbols @maxStringLength @(h + 1) cfg taskFnDataset

--  shape synthesizer
-- , KnownShape shape, Synthesizer device shape rules synthesizer
getM :: forall device featMult m rules maxChar symbols maxStringLength h . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat m, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h) => SynthesizerConfig -> TaskFnDataset -> [IO ()]
getM cfg taskFnDataset = let
    SynthesizerConfig{..} = cfg
    TaskFnDataset{..} = taskFnDataset
    variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> exprBlocks
    in (:)
        -- (void . interpretUnsafe $ train @device cfg taskFnDataset model)
        (case synthesizer of
            "random" -> do
                model <- A.sample RandomSynthesizerSpec
                void . interpretUnsafe $ train @device @rules @'[] @0 @RandomSynthesizer cfg taskFnDataset model
            "nsps" -> do
                model <- A.sample spec
                void . interpretUnsafe $ train @device @rules @'[R3nnBatch, maxStringLength * (2 * featMult * Dirs * h)] @(maxStringLength * m) @(NSPS device m symbols rules maxStringLength EncoderBatch R3nnBatch maxChar h featMult) cfg taskFnDataset model
                where
                variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> exprBlocks
                useTypes = natValI @featMult > 1
                charMap = if useTypes then bothCharMap else exprCharMap
                encoder_spec :: LstmEncoderSpec device maxStringLength EncoderBatch maxChar h featMult =
                    LstmEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropoutRate
                r3nn_spec :: R3NNSpec device m symbols rules maxStringLength R3nnBatch h maxChar featMult =
                    initR3nn variants r3nnBatch dropoutRate charMap
                rule_encoder_spec :: TypeEncoderSpec device maxStringLength maxChar m =
                    TypeEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropoutRate
                spec :: NSPSSpec device m symbols rules maxStringLength EncoderBatch R3nnBatch maxChar h featMult =
                    NSPSSpec encoder_spec rule_encoder_spec r3nn_spec
            _ -> error "synthesizer not recognized")
        $ getM @device @featMult @(m + 1) @rules @maxChar @symbols @maxStringLength @h cfg taskFnDataset
