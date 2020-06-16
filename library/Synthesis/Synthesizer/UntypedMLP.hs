{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Synthesis.Synthesizer.UntypedMLP (
    module Synthesis.Synthesizer.UntypedMLP
) where

import GHC.Generics
import Prelude hiding (exp, tanh)
import Torch

data MLPSpec = MLPSpec {
    inputFeatures :: Int,
    -- hiddenFeatures0 :: Int,
    -- hiddenFeatures1 :: Int,
    outputFeatures :: Int
    } deriving (Show, Eq)

data MLP = MLP {
    l0 :: Linear
    -- l0 :: Linear,
    -- l1 :: Linear,
    -- l2 :: Linear
    } deriving (Generic, Show)

instance Parameterized MLP
instance Randomizable MLPSpec MLP where
    sample MLPSpec {..} = MLP 
        -- <$> sample (LinearSpec inputFeatures hiddenFeatures0)
        -- <*> sample (LinearSpec hiddenFeatures0 hiddenFeatures1)
        -- <*> sample (LinearSpec hiddenFeatures1 outputFeatures)
        <$> sample (LinearSpec inputFeatures outputFeatures)

mlp :: MLP -> Tensor -> Tensor
mlp MLP{..} input = 
    -- logSoftmax (Dim 1)
    -- . linear l2
    -- . tanh
    -- . linear l1
    . tanh
    . linear l0
    $ input
