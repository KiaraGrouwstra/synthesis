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

-- | actually Char seems in Int range, i.e. [-2^29 .. 2^29-1]... I think I wouldn't need more than ascii tho.
-- TODO: see if I can further trim this down
type MaxChar = 256
max_char :: Int
max_char = natValI @MaxChar

-- | number of features for R3NN expansions/symbols. must be an even number for H.
type M = 20
m :: Int
m = natValI @M

-- | must use a static batch size i/o making it dynamic by SynthesizerConfig...
type BatchSize = 8
batchSize :: Int
batchSize = natValI @BatchSize

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
type Rules = 15
type MaxStringLength = 14
type N_train = 7
type N_validation = 2
type N_test = 6
type RhsSymbols = 6
type Symbols = LhsSymbols + RhsSymbols
