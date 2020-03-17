-- adapted from: https://github.com/hasktorch/hasktorch/blob/master/examples/static-mnist-mlp/Main.hs

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Synthesis.Synthesizer.TypedMLP (
    MLPSpec (..),
    MLP (..),
    mlp,
) where

import           Prelude                 hiding ( tanh )
import           Control.Exception.Safe         ( try
                                                , SomeException(..)
                                                )
import           Control.Monad                  ( foldM
                                                , when
                                                )
import           Data.Proxy
import           Foreign.ForeignPtr
import           GHC.Generics
import           GHC.TypeLits
import           GHC.TypeLits.Extra
import           System.Environment
import           System.IO.Unsafe
import           System.Random

-- import qualified Torch.Internal.Cast                     as ATen
-- import qualified Torch.Internal.Class                    as ATen
-- import qualified Torch.Internal.Type                     as ATen
-- import qualified Torch.Internal.Managed.Type.Tensor      as ATen
-- import qualified Torch.Internal.Managed.Type.Context     as ATen
import           Torch.Typed.Aux
import           Torch.Typed.Tensor
import           Torch.Typed.Functional      hiding ( linear
                                                , dropout
                                                )
import           Torch.Typed.Factories
import           Torch.Typed.NN
import           Torch.Typed.Optim
import           Torch.Typed.Parameter
import qualified Torch.Autograd                as A
import qualified Torch.NN                      as A
import qualified Torch.Device                  as D
import qualified Torch.DType                   as D
import qualified Torch.Tensor                  as D
import qualified Torch.Functional              as D
import qualified Torch.TensorFactories         as D
import qualified Torch.Serialize               as D
import qualified Torch.Typed.Vision            as I

--------------------------------------------------------------------------------
-- MLP for MNIST
--------------------------------------------------------------------------------

data MLPSpec (inputFeatures :: Nat) (outputFeatures :: Nat)
             (hiddenFeatures0 :: Nat) (hiddenFeatures1 :: Nat)
             (dtype :: D.DType)
             (device :: (D.DeviceType, Nat))
 where
  MLPSpec
    :: forall inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
     . { mlpDropoutProbSpec :: Double }
    -> MLPSpec inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
 deriving (Show, Eq)

data MLP (inputFeatures :: Nat) (outputFeatures :: Nat)
         (hiddenFeatures0 :: Nat) (hiddenFeatures1 :: Nat)
         (dtype :: D.DType)
         (device :: (D.DeviceType, Nat))
 where
  MLP
    :: forall inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
     . { mlpLayer0  :: Linear inputFeatures   hiddenFeatures0 dtype device
       , mlpLayer1  :: Linear hiddenFeatures0 hiddenFeatures1 dtype device
       , mlpLayer2  :: Linear hiddenFeatures1 outputFeatures  dtype device
       , mlpDropout :: Dropout
       }
    -> MLP inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device
 deriving (Show, Generic)

instance ( KnownNat inputFeatures
         , KnownNat outputFeatures
         , KnownNat hiddenFeatures0
         , KnownNat hiddenFeatures1
         , KnownDType dtype
         , KnownDevice device
         , RandDTypeIsValid device dtype
         )
  => A.Randomizable (MLPSpec inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device)
                    (MLP     inputFeatures outputFeatures hiddenFeatures0 hiddenFeatures1 dtype device)
 where
  sample MLPSpec {..} =
    MLP
      <$> A.sample LinearSpec
      <*> A.sample LinearSpec
      <*> A.sample LinearSpec
      <*> A.sample (DropoutSpec mlpDropoutProbSpec)

mlp
  :: forall
       batchSize
       inputFeatures outputFeatures
       hiddenFeatures0 hiddenFeatures1
       dtype
       device
   . (StandardFloatingPointDTypeValidation device dtype)
  => MLP inputFeatures outputFeatures
         hiddenFeatures0 hiddenFeatures1
         dtype
         device
  -> Bool
  -> Tensor device dtype '[batchSize, inputFeatures]
  -> IO (Tensor device dtype '[batchSize, outputFeatures])
mlp MLP {..} train input =
  return
    .   linear mlpLayer2
    =<< dropout mlpDropout train
    .   tanh
    .   linear mlpLayer1
    =<< dropout mlpDropout train
    .   tanh
    .   linear mlpLayer0
    =<< pure input
