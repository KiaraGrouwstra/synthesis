{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.Encoder (module Synthesis.Synthesizer.Encoder) where

import Data.Bifunctor (second, bimap)
import Data.Int (Int64) 
import Data.Char (ord)
import Data.HashMap.Lazy (HashMap, (!), toList)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat, KnownNat, type (*))
import Util (fstOf3)

import           Torch.Typed.Tensor
import qualified Torch.Typed.Tensor
import           Torch.Typed.Functional
import           Torch.Typed.Factories
import           Torch.Typed.Aux
import           Torch.Typed.Parameter
import qualified Torch.Typed.Parameter
import           Torch.Autograd
import           Torch.HList
import           Torch.Scalar
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Functional.Internal     as I
import qualified Torch.Tensor                  as D
import qualified Torch.Device                  as D
import qualified Torch.DType                   as D
import           Torch.Typed.NN
import           Torch.Typed.NN.Recurrent.LSTM

import Synthesis.Orphanage ()
import Synthesis.Data (Expr, Tp, Tpl2)
import Synthesis.Utility (pp, mapBoth, asPairs)
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.Params

data LstmEncoderSpec
    (device :: (D.DeviceType, Nat))
    (maxStringLength :: Nat)
    (batch_size :: Nat)
    (maxChar :: Nat)
    (h :: Nat)
 where LstmEncoderSpec :: {
        charMap :: HashMap Char Int,
        lstmSpec :: LSTMSpec maxChar h NumLayers Dir 'D.Float device
    } -> LstmEncoderSpec device maxStringLength batch_size maxChar h
 deriving (Show)

data LstmEncoder
    (device :: (D.DeviceType, Nat))
    (maxStringLength :: Nat)
    (batch_size :: Nat)
    (maxChar :: Nat)
    (h :: Nat)
 where LstmEncoder :: {
    charMap :: HashMap Char Int,
    inModel  :: LSTMWithInit maxChar h NumLayers Dir 'ConstantInitialization 'D.Float device,
    outModel :: LSTMWithInit maxChar h NumLayers Dir 'ConstantInitialization 'D.Float device
    } -> LstmEncoder device maxStringLength batch_size maxChar h
 deriving (Show, Generic)

-- instance (Scalar a) => A.Parameterized a where
instance A.Parameterized Int where
  flattenParameters _ = []
  replaceOwnParameters = return

instance A.Parameterized (LstmEncoder device maxStringLength batch_size maxChar h)

instance (KnownDevice device, RandDTypeIsValid device 'D.Float, KnownNat maxChar, KnownNat h) => A.Randomizable (LstmEncoderSpec device maxStringLength batch_size maxChar h) (LstmEncoder device maxStringLength batch_size maxChar h) where
    sample LstmEncoderSpec {..} = do
        in_model  :: LSTMWithInit maxChar h NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
        out_model :: LSTMWithInit maxChar h NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
        return $ LstmEncoder charMap in_model out_model
            -- TODO: consider LearnedInitialization
            where spec :: LSTMWithInitSpec maxChar h NumLayers Dir 'ConstantInitialization 'D.Float device = LSTMWithZerosInitSpec lstmSpec

-- instance (KnownDevice device, KnownNat batch_size, KnownNat n', KnownNat maxStringLength, KnownNat maxChar, KnownNat h, shape ~ '[n', maxStringLength * (4 * Dirs * h)])
--     => HasForward (LstmEncoder device maxStringLength batch_size maxChar h) [(Expr, Either String Expr)] (Tensor device 'D.Float shape) where
--         forward      = lstmEncoder
--         -- forwardStoch = lstmEncoder

lstmBatch
    :: forall batch_size maxStringLength maxChar device h
     . (KnownNat batch_size, KnownNat maxStringLength, KnownNat maxChar, KnownNat h)
    => LstmEncoder device maxStringLength batch_size maxChar h
    -> Tensor device 'D.Float '[batch_size, 2 * maxStringLength, maxChar]
    -> Tensor device 'D.Float '[batch_size, 2 * maxStringLength, maxChar]
    -> Tensor device 'D.Float '[batch_size, maxStringLength * (4 * Dirs * h)]
lstmBatch LstmEncoder{..} in_vec out_vec = feat_vec where
    lstm' = \model -> fstOf3 . lstmWithDropout @'BatchFirst model
    emb_in  :: Tensor device 'D.Float '[batch_size, 2 * maxStringLength, h * Dirs] = lstm'  inModel  in_vec
    emb_out :: Tensor device 'D.Float '[batch_size, 2 * maxStringLength, h * Dirs] = lstm' outModel out_vec
    -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    feat_vec :: Tensor device 'D.Float '[batch_size, maxStringLength * (4 * Dirs * h)] =
            -- reshape $ cat @2 $ emb_in :. emb_out :. HNil
            asUntyped (D.reshape [natValI @batch_size, natValI @maxStringLength * (4 * natValI @Dirs * natValI @h)]) $ cat @2 $ emb_in :. emb_out :. HNil

-- | NSPS paper's Baseline LSTM encoder
lstmEncoder
    :: forall batch_size maxStringLength maxChar n' device h featTnsr
     . (KnownDevice device, KnownNat batch_size, KnownNat maxStringLength, KnownNat maxChar, KnownNat h, featTnsr ~ Tensor device 'D.Float '[maxStringLength, maxChar])
    => LstmEncoder device maxStringLength batch_size maxChar h
    -> HashMap (Tp, Tp) [(Expr, Either String Expr)]
    -> Tensor device 'D.Float '[n', maxStringLength * (4 * Dirs * h)]
lstmEncoder encoder tp_io_pairs = UnsafeMkTensor feat_vec where
    LstmEncoder{..} = encoder
    maxStringLength_ :: Int = natValI @maxStringLength
    batch_size_ :: Int = natValI @batch_size
    max_char :: Int = natValI @maxChar

    -- TODO: use tree encoding (R3NN) also for expressions instead of just converting to string
    str_map :: HashMap (Tpl2 String) [(Tpl2 String)] =
            bimap (mapBoth pp) (fmap (bimap pp (show . second pp))) `asPairs` tp_io_pairs
    -- convert char to one-hot encoding (byte -> 256 1/0s as float) as third lstm dimension
    str2tensor :: Int -> String -> featTnsr =
            \len -> Torch.Typed.Tensor.toDType @'D.Float . UnsafeMkTensor . D.toDevice (deviceVal @device) . (`I.one_hot` max_char) . D.asTensor . padRight 0 len . fmap ((fromIntegral :: Int -> Int64) . (+1) . (!) charMap)

    both2t :: Tpl2 String -> Tpl2 featTnsr = mapBoth $ str2tensor maxStringLength_
    addTypes :: (featTnsr, [featTnsr]) -> D.Tensor =
        \(tp, vecs) -> F.cat (F.Dim 1) [stack' 0 (toDynamic <$> vecs), repeatDim 0 (length vecs) (toDynamic tp)]
    tp_ios :: [(Tpl2 featTnsr, [Tpl2 featTnsr])] = (bimap both2t $ fmap both2t) <$> toList str_map
    vec_pairs :: [(D.Tensor, D.Tensor)] = (\((in_tp, out_tp), ios) -> let (ins, outs) = unzip ios in addTypes `mapBoth` ((in_tp, ins), (out_tp, outs))) <$> tp_ios
    (in_vecs, out_vecs) :: (Tpl2 [Tensor device 'D.Float '[batch_size, 2 * maxStringLength, maxChar]]) =
            mapBoth (fmap UnsafeMkTensor . batchTensor batch_size_ . F.cat (F.Dim 0)) . unzip $ vec_pairs
    feat_vecs :: [Tensor device 'D.Float '[batch_size, maxStringLength * (4 * Dirs * h)]] =
            uncurry (lstmBatch encoder) <$> zip in_vecs out_vecs
    feat_vec :: D.Tensor = F.cat (F.Dim 0) $ toDynamic <$> feat_vecs

-- | 5.1.2 Cross Correlation encoder

-- | To help the model discover input substrings that are copied to the output, we designed an novel I/O example encoder to compute the cross correlation between each input and output example representation.
-- | We used the two output tensors of the LSTM encoder (discussed above) as inputs to this encoder.
-- | For each example pair, we first slide the output feature block over the input feature block and compute the dot product between the respective position representation.
-- | Then, we sum over all overlapping time steps.
-- | Features of all pairs are then concatenated to form a 2∗(T−1)-dimensional vector encoding for all example pairs.
-- | There are 2∗(T−1) possible alignments in total between input and output feature blocks.

-- Cross Correlation encoder
-- rotate, rotateT
-- h1_in, h1_out
-- dot(a, b)
-- mm(a, b)
-- matmul(a, b) performs matrix multiplications if both arguments are 2D and computes their dot product if both arguments are 1D
-- bmm(a, b)
-- bdot(a, b): (a*b).sum(-1)  -- https://github.com/pytorch/pytorch/issues/18027
-- htan activation fn

-- | We also designed the following variants of this encoder.

-- | Diffused Cross Correlation Encoder:
-- | This encoder is identical to the Cross Correlation encoder except that instead of summing over overlapping time steps after the element-wise dot product, we simply concatenate the vectors corresponding to all time steps, resulting in a final representation that contains 2∗(T−1)∗T features for each example pair.

-- | LSTM-Sum Cross Correlation Encoder:
-- | In this variant of the Cross Correlation encoder, instead of doing an element-wise dot product, we run a bidirectional LSTM over the concatenated feature blocks of each alignment.
-- | We represent each alignment by the LSTM hidden representation of the final time step leading to a total of 2∗H∗2∗(T−1) features for each example pair.

-- | Augmented Diffused Cross Correlation Encoder:
-- | For this encoder, the output of each character position of the Diffused Cross Correlation encoder is combined with the character embedding at this position, then a basic LSTM encoder is run over the combined features to extract a 4∗H-dimensional vector for both the input and the output streams.
-- | The LSTM encoder output is then concatenated with the output of the Diffused Cross Correlation encoder forming a (4∗H+T∗(T−1))-dimensional feature vector for each example pair.
