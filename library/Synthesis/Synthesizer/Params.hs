{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Synthesis.Synthesizer.Params (module Synthesis.Synthesizer.Params) where

import GHC.TypeNats (type (+))
import Torch.Typed.Aux

-- TODO: consider which hyperparams have been / should be shared across networks

-- | left-hand symbols: just Expression in our lambda-calculus DSL
type LhsSymbols = 1

-- | number of features for R3NN expansions/symbols. must be an even number for H.
type M = 20
m :: Int
m = natValI @M

-- | must use a static batch size i/o making it dynamic by SynthesizerConfig...
type EncoderBatch = 8
encoderBatch :: Int
encoderBatch = natValI @EncoderBatch
type R3nnBatch = 8
r3nnBatch :: Int
r3nnBatch = natValI @R3nnBatch

-- left/right MLPs
type Hidden0 = 20
hidden0 :: Int
hidden0 = natValI @Hidden0
type Hidden1 = 20
hidden1 :: Int
hidden1 = natValI @Hidden1

-- Encoder
-- | H is the topmost LSTM hidden dimension
type H = 20  -- H=30 makes the encoder loss improvement test fail; why?
h :: Int
h = natValI @H

-- R3NN
type NumLayers = 3

-- type RhsSymbols = 36  -- Tamandu
-- type Rules = 92       -- Tamandu
type Rules = 9
type MaxStringLength = 11
type N_train = 30
type N_validation = 7
type N_test = 29
type RhsSymbols = 4
type Symbols = LhsSymbols + RhsSymbols
type MaxChar = 19
