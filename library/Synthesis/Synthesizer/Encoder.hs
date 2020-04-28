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
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.Encoder (module Synthesis.Synthesizer.Encoder) where

import Data.Bifunctor (first, second)
import Data.Int (Int64) 
import Data.Char (ord)
import Data.HashMap.Lazy (HashMap, (!))
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
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Functional.Internal     as I
import qualified Torch.Tensor                  as D
import qualified Torch.Device                  as D
import qualified Torch.DType                   as D
import           Torch.Typed.NN.Recurrent.LSTM

import Synthesis.Orphanage ()
import Synthesis.Data (Expr)
import Synthesis.Utility (pp, mapTuple)
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.Params

data LstmEncoderSpec
    (device :: (D.DeviceType, Nat))
    (t :: Nat)
    (batch_size :: Nat)
    (maxChar :: Nat)
 where LstmEncoderSpec :: {
        lstmSpec :: LSTMSpec maxChar H NumLayers Dir 'D.Float device
    } -> LstmEncoderSpec device t batch_size maxChar
 deriving (Show)

data LstmEncoder
    (device :: (D.DeviceType, Nat))
    (t :: Nat)
    (batch_size :: Nat)
    (maxChar :: Nat)
 where LstmEncoder :: {
    inModel  :: LSTMWithInit maxChar H NumLayers Dir 'ConstantInitialization 'D.Float device,
    outModel :: LSTMWithInit maxChar H NumLayers Dir 'ConstantInitialization 'D.Float device
    } -> LstmEncoder device t batch_size maxChar
 deriving (Show, Generic)

instance A.Parameterized (LstmEncoder device t batch_size maxChar)

instance (KnownDevice device, RandDTypeIsValid device 'D.Float, KnownNat maxChar) => A.Randomizable (LstmEncoderSpec device t batch_size maxChar) (LstmEncoder device t batch_size maxChar) where
    sample LstmEncoderSpec {..} = do
        in_model  :: LSTMWithInit maxChar H NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
        out_model :: LSTMWithInit maxChar H NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
        return $ LstmEncoder in_model out_model
        -- <$> A.sample spec
        -- <*> A.sample spec
            -- TODO: consider LearnedInitialization
            where spec :: LSTMWithInitSpec maxChar H NumLayers Dir 'ConstantInitialization 'D.Float device = LSTMWithZerosInitSpec lstmSpec

lstmBatch
    :: forall batch_size t maxChar n n' device
     . (KnownNat batch_size, KnownNat t, KnownNat maxChar)
    => LstmEncoder device t batch_size maxChar
    -> Tensor device 'D.Float '[batch_size, t, maxChar]
    -> Tensor device 'D.Float '[batch_size, t, maxChar]
    -> Tensor device 'D.Float '[batch_size, t * (2 * Dirs * H)]
lstmBatch LstmEncoder{..} in_vec out_vec = feat_vec where
    lstm' = \model -> fstOf3 . lstmWithDropout @'BatchFirst model
    emb_in  :: Tensor device 'D.Float '[batch_size, t, Dirs * H] = lstm'  inModel  in_vec
    emb_out :: Tensor device 'D.Float '[batch_size, t, Dirs * H] = lstm' outModel out_vec
    -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    feat_vec :: Tensor device 'D.Float '[batch_size, t * (2 * Dirs * H)] =
            reshape $ cat @2 $ emb_in :. emb_out :. HNil

-- | NSPS paper's Baseline LSTM encoder
lstmEncoder
    :: forall batch_size t maxChar n n' device
     . (KnownDevice device, KnownNat batch_size, KnownNat t, KnownNat maxChar)
    => LstmEncoder device t batch_size maxChar
    -> HashMap Char Int
    -> [(Expr, Either String Expr)]
    -> IO (Tensor device 'D.Float '[n', t * (2 * Dirs * H)])
lstmEncoder encoder charMap io_pairs = do
    let LstmEncoder{..} = encoder
    let t_ :: Int = natValI @t
    let batch_size_ :: Int = natValI @batch_size
    let max_char :: Int = natValI @maxChar

    -- TODO: use tree encoding (R3NN) also for expressions instead of just converting to string
    let str_pairs :: [(String, String)] = first pp . second (show . second pp) <$> io_pairs
    -- convert char to one-hot encoding (byte -> 256 1/0s as float) as third lstm dimension
    let str2tensor :: Int -> String -> Tensor device 'D.Float '[1, t, maxChar] = \len -> Torch.Typed.Tensor.toDType @'D.Float . UnsafeMkTensor . D.toDevice (deviceVal @device) . flip I.one_hot max_char . D.asTensor . padRight 0 len . fmap ((fromIntegral :: Int -> Int64) . (+1) . (!) charMap)
    let vec_pairs :: [(Tensor device 'D.Float '[1, t, maxChar], Tensor device 'D.Float '[1, t, maxChar])] = first (str2tensor t_) . second (str2tensor t_) <$> str_pairs
    putStrLn $ "vec_pairs:\n" <> show (mapTuple tensorStats <$> vec_pairs)

    -- pre-vectored
    -- stack input vectors and pad to static dataset size
    let stackPad :: [D.Tensor] -> [Tensor device 'D.Float '[batch_size, t, maxChar]] =
            fmap UnsafeMkTensor . batchTensor batch_size_ . stack' 0
            -- batchTensor' @0 . UnsafeMkTensor . stack' 0  -- ambiguous shape
    let  in_vecs :: [Tensor device 'D.Float '[batch_size, t, maxChar]] =
            stackPad $ toDynamic . fst <$> vec_pairs
    putStrLn $ "in_vecs:\n" <> show (tensorStats <$> in_vecs)
    let out_vecs :: [Tensor device 'D.Float '[batch_size, t, maxChar]] =
            stackPad $ toDynamic . snd <$> vec_pairs
    -- putStrLn $ "out_vecs"
    putStrLn $ "out_vecs:\n" <> show (tensorStats <$> out_vecs)

    let feat_vecs :: [Tensor device 'D.Float '[batch_size, t * (2 * Dirs * H)]] =
            uncurry (lstmBatch encoder) <$> zip in_vecs out_vecs
    -- putStrLn $ "feat_vecs"
    putStrLn $ "feat_vecs:\n" <> show (tensorStats <$> feat_vecs)
    let feat_vec :: Tensor device 'D.Float '[n', t * (2 * Dirs * H)] =
            UnsafeMkTensor $ F.cat (F.Dim 0) $ toDynamic <$> feat_vecs
    putStrLn $ "feat_vec:\n" <> show (tensorStats feat_vec)
    return feat_vec

    -- -- mapped: I'm not quite sure if this is learning across samples as the lstms seem not updated? should it??
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float device = LSTMSpec dropoutSpec
    -- let spec :: Spec = LSTMWithZerosInitSpec lstm_spec
    -- in_lstm  :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
    -- out_lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
    -- let feat_vecs :: [Tensor device 'D.Float '[2 * Dirs * H * t]] = flip fmap vec_pairs $ \(tensor_in, tensor_out) -> let
    --             (emb_in, hidden_in, cell_in) ::
    --                 (Tensor device 'D.Float '[1, t, Dirs * H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, 1, H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, 1, H])
    --                     = lstmWithDropout @'BatchFirst in_lstm tensor_in
    --             (emb_out, hidden_out, cell_out) ::
    --                 (Tensor device 'D.Float '[1, t, Dirs * H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, 1, H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, 1, H])
    --                     = lstmWithDropout @'BatchFirst out_lstm tensor_out
    --             -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --             -- TODO: check if this remotely makes sense cuz I have no clue
    --             feat :: Tensor device 'D.Float '[1, t, 2 * Dirs * H * t] = cat @2 $ emb_in :. emb_out :. HNil
    --             -- flatten results at the end
    --             -- feat_ :: Tensor device 'D.Float '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat
    --         in feat
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor device 'D.Float '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * Dirs * h * t]) feat_vec

    -- -- sequential
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float device = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tensor device 'D.Float '[1, t, maxChar] -> IO (Tensor device 'D.Float '[1, t, Dirs * H], Spec) = \ spec tensor -> do
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
    --         let (emb, hidden, cell) :: (
    --             Tensor device 'D.Float '[1, t, Dirs * H],
    --             Tensor device 'D.Float '[Dirs * NumLayers, H],
    --             Tensor device 'D.Float '[Dirs * NumLayers, H])
    --                 = lstmWithDropout @'BatchFirst lstm tensor
    --         let spec_ = LSTMWithConstInitSpec lstm_spec cell hidden
    --         return (emb, spec_)
    -- let f :: (Tensor device 'D.Float '[1, t, maxChar], Tensor device 'D.Float '[1, t, maxChar])
    --         -> ([Tensor device 'D.Float '[1, t, 2 * Dirs * H * t]], Spec, Spec)
    --         -> IO ([Tensor device 'D.Float '[1, t, 2 * Dirs * H * t]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tensor device 'D.Float '[1, t, 2 * Dirs * H * t] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tensor device 'D.Float '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat

    --                 return (feat : feats, spec_in_, spec_out_)
    -- feat_vecs :: [Tensor device 'D.Float '[2 * Dirs * H * t]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor device 'D.Float '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- -- sequential: hidden, t as N
    -- let lstm_spec :: LSTMSpec t H NumLayers Dir 'D.Float device = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tensor device 'D.Float '[t, 1, maxChar] -> IO (Tensor device 'D.Float '[Dirs, t, H], Spec) = \ spec tensor -> do
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
    --         let (_emb, hidden, cell) :: (
    --                 Tensor device 'D.Float '[t, 1, Dirs * H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, t, H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, t, H])
    --                     = lstmWithDropout @'BatchFirst lstm tensor
    --         let spec_ :: Spec = LSTMWithConstInitSpec lstm_spec cell hidden
    --         let last_hidden :: Tensor device 'D.Float '[Dirs, t, H] = assert (dirs == 2) $ stack @0 (select @0 @(NumLayers - 1) hidden :. select @0 @NumLayers hidden :. HNil)  -- is it really the last two?
    --         return (last_hidden, spec_)
    -- let f :: (Tensor device 'D.Float '[t, 1, maxChar], Tensor device 'D.Float '[t, 1, maxChar])
    --         -> ([Tensor device 'D.Float '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         -> IO ([Tensor device 'D.Float '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tensor device 'D.Float '[t, 1, 2 * Dirs * H] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tensor device 'D.Float '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat
    --                 return (feat : feats, spec_in_, spec_out_)
    -- -- feat_vecs :: [Tensor device 'D.Float '[2 * Dirs * H * t]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- (feat_vecs, _spec_in, _spec_out) :: ([Tensor device 'D.Float '[t, 1, 2 * Dirs * H]], Spec, Spec) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor device 'D.Float '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- -- sequential: hidden, loop over t
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float device = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tensor device 'D.Float '[1, 1, maxChar] -> IO (Tensor device 'D.Float '[Dirs, 1, H], Spec) = \ spec tensor -> do
    --         -- tensor :: Tensor device 'D.Float '[1, 1, maxChar] = asUntyped (select'' 0 0) tensor_in  -- asUntyped'
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float device <- A.sample spec
    --         let (_emb, hidden, cell) :: (
    --                 Tensor device 'D.Float '[1, 1, Dirs * H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, 1, H],
    --                 Tensor device 'D.Float '[Dirs * NumLayers, 1, H])
    --                     = lstmWithDropout @'BatchFirst lstm tensor
    --         let spec_ :: Spec = LSTMWithConstInitSpec lstm_spec cell hidden
    --         let last_hidden :: Tensor device 'D.Float '[Dirs, 1, H] = assert (dirs == 2) $ stack @0 (select @0 @(Dirs * (NumLayers-1)) hidden :. select @0 @(Dirs * NumLayers - 1) hidden :. HNil)  -- last two?
    --         -- let last_hidden :: Tensor device 'D.Float '[Dirs, 1, H] = assert (dirs == 2) $ stack @0 (select @0 @((Dirs-1) * NumLayers - 1) hidden :. select @0 @(Dirs * NumLayers - 1) hidden :. HNil)  -- last interspersed?
    --         return (last_hidden, spec_)
    -- let f :: (Tensor device 'D.Float '[t, 1, maxChar], Tensor device 'D.Float '[t, 1, maxChar])
    --         -> ([Tensor device 'D.Float '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         -> IO ([Tensor device 'D.Float '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 -- (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_in, spec_in_) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    --                 select @1 @i
    --                 -- (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- ???
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tensor device 'D.Float '[t, 1, 2 * Dirs * H] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tensor device 'D.Float '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat
    --                 return (feat : feats, spec_in_, spec_out_)
    -- -- feat_vecs :: [Tensor device 'D.Float '[2 * Dirs * H * t]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- (feat_vecs, _spec_in, _spec_out) :: ([Tensor device 'D.Float '[t, 1, 2 * Dirs * H]], Spec, Spec) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tensor device 'D.Float '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor device 'D.Float '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * dirs * h * t]) feat_vec

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
