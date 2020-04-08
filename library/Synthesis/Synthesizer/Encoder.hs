{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoStarIsType #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.Encoder (module Synthesis.Synthesizer.Encoder) where

-- import GHC.Exts (fromList)
import Data.Bifunctor (first, second)
-- import Control.Exception (assert)
import Data.Int (Int64) 
import Data.Char (ord)
-- import Data.Foldable (foldrM)
-- import qualified GHC.TypeNats
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, type (*)) -- , Nat, Mod, type (+), type (-)
import Util (fstOf3)

import Torch.Typed.Tensor
import Torch.Typed.Functional
-- import Torch.Typed.NN
-- import Torch.Typed.Factories
import Torch.Typed.Aux
import Torch.HList
-- import qualified Torch.HList
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Functional.Internal     as I
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
-- import qualified Torch.Device                  as D
import Torch.Typed.NN.Recurrent.LSTM

import Synthesis.Data (Expr)
import Synthesis.Utility (pp) -- , pp_
import Synthesis.Synthesizer.Utility -- (Dev, Dir, Dirs, asUntyped', padRight, select', rotate) -- rotateT
-- import qualified Synthesis.Synthesizer.UntypedMLP as UntypedMLP

type NumLayers = 3 -- ?

-- | H is the topmost LSTM hidden dimension
type H = 30 -- ?
h :: Int
h = natValI @H

data BaselineLstmEncoderSpec = BaselineLstmEncoderSpec {
        -- dropoutRate :: Double
        lstmSpec :: LSTMSpec MaxChar H NumLayers Dir 'D.Float Dev
    } deriving (Show)  -- , Eq

data BaselineLstmEncoder = BaselineLstmEncoder {
    in_model  :: LSTMWithInit MaxChar H NumLayers Dir 'ConstantInitialization 'D.Float Dev,
    out_model :: LSTMWithInit MaxChar H NumLayers Dir 'ConstantInitialization 'D.Float Dev
    } deriving (Show, Generic)

instance () => A.Parameterized (BaselineLstmEncoder) where
  flattenParameters BaselineLstmEncoder{..} = []
        --    A.flattenParameters  in_model
        -- ++ A.flattenParameters out_model
  replaceOwnParameters = pure
--   replaceOwnParameters BaselineLstmEncoder{..} = do
--     out_model' <- A.replaceOwnParameters out_model
--     in_model'  <- A.replaceOwnParameters  in_model
--     return $ BaselineLstmEncoder
--                  {  in_model =  in_model'
--                  , out_model = out_model'
--                  }

instance A.Randomizable BaselineLstmEncoderSpec BaselineLstmEncoder where
    sample BaselineLstmEncoderSpec {..} = BaselineLstmEncoder
        <$> A.sample spec
        <*> A.sample spec
            where spec = LSTMWithZerosInitSpec lstmSpec

-- | 5.1.1 Baseline LSTM encoder
-- | This encoding is conceptually straightforward and has very little prior knowledge about what operations are being performed over the strings, i.e., substring, constant, etc., which might make it difficult to discover substring indices, especially the ones based on regular expressions.

baselineLstmEncoder
    :: forall batch_size t
     . (KnownNat batch_size, KnownNat t)
    => BaselineLstmEncoder
    -> [(Expr, Either String Expr)]
    -> IO (Tnsr '[batch_size, t * (2 * Dirs * H)])
baselineLstmEncoder BaselineLstmEncoder{..} io_pairs = do
    let t_ :: Int = natValI @t
    let batch_size_ :: Int = natValI @batch_size
    let n_ :: Int = length io_pairs

    -- TODO: use tree encoding (R3NN) also for expressions instead of just converting to string
    let str_pairs :: [(String, String)] = first pp . second (show . second pp) <$> io_pairs  -- second pp_
    -- | Our first I/O encoding network involves running two separate deep bidirectional LSTM networks for processing the input and the output string in each example pair.
    -- convert char to one-hot encoding (byte -> 256 1/0s as float) as third lstm dimension
    let str2tensor :: Int -> String -> Tnsr '[1, t, MaxChar] = \len -> toDType @'D.Float . UnsafeMkTensor . flip I.one_hot max_char . D.asTensor . padRight 0 len . fmap ((fromIntegral :: Int -> Int64) . ord)
    let vec_pairs :: [(Tnsr '[1, t, MaxChar], Tnsr '[1, t, MaxChar])] = first (str2tensor t_) . second (str2tensor t_) <$> str_pairs
    -- print $ "vec_pairs: " ++ show (first (show . D.shape . toDynamic) . second (show . D.shape . toDynamic) <$> vec_pairs)

    -- -- mapped: I'm not quite sure if this is learning across samples as the lstms seem not updated? should it??
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let spec :: Spec = LSTMWithZerosInitSpec lstm_spec
    -- in_lstm  :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    -- out_lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    -- let feat_vecs :: [Tnsr '[2 * Dirs * H * t]] = flip fmap vec_pairs $ \(tensor_in, tensor_out) -> let
    --             (emb_in, hidden_in, cell_in) ::
    --                 (Tnsr '[1, t, Dirs * H],
    --                 Tnsr '[Dirs * NumLayers, 1, H],
    --                 Tnsr '[Dirs * NumLayers, 1, H])
    --                     = lstmWithDropout @'BatchFirst in_lstm tensor_in
    --             (emb_out, hidden_out, cell_out) ::
    --                 (Tnsr '[1, t, Dirs * H],
    --                 Tnsr '[Dirs * NumLayers, 1, H],
    --                 Tnsr '[Dirs * NumLayers, 1, H])
    --                     = lstmWithDropout @'BatchFirst out_lstm tensor_out
    --             -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --             -- TODO: check if this remotely makes sense cuz I have no clue
    --             feat :: Tnsr '[1, t, 2 * Dirs * H * t] = cat @2 $ emb_in :. emb_out :. HNil
    --             -- flatten results at the end
    --             -- feat_ :: Tnsr '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat
    --         in feat
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tnsr '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * Dirs * h * t]) feat_vec

    -- -- sequential
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tnsr '[1, t, MaxChar] -> IO (Tnsr '[1, t, Dirs * H], Spec) = \ spec tensor -> do
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    --         let (emb, hidden, cell) :: (
    --             Tnsr '[1, t, Dirs * H],
    --             Tnsr '[Dirs * NumLayers, H],
    --             Tnsr '[Dirs * NumLayers, H])
    --                 = lstmWithDropout @'BatchFirst lstm tensor
    --         let spec_ = LSTMWithConstInitSpec lstm_spec cell hidden
    --         return (emb, spec_)
    -- let f :: (Tnsr '[1, t, MaxChar], Tnsr '[1, t, MaxChar])
    --         -> ([Tnsr '[1, t, 2 * Dirs * H * t]], Spec, Spec)
    --         -> IO ([Tnsr '[1, t, 2 * Dirs * H * t]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tnsr '[1, t, 2 * Dirs * H * t] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tnsr '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat

    --                 return (feat : feats, spec_in_, spec_out_)
    -- feat_vecs :: [Tnsr '[2 * Dirs * H * t]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tnsr '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- -- sequential: hidden, t as N
    -- let lstm_spec :: LSTMSpec t H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tnsr '[t, 1, MaxChar] -> IO (Tnsr '[Dirs, t, H], Spec) = \ spec tensor -> do
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    --         let (_emb, hidden, cell) :: (
    --                 Tnsr '[t, 1, Dirs * H],
    --                 Tnsr '[Dirs * NumLayers, t, H],
    --                 Tnsr '[Dirs * NumLayers, t, H])
    --                     = lstmWithDropout @'BatchFirst lstm tensor
    --         let spec_ :: Spec = LSTMWithConstInitSpec lstm_spec cell hidden
    --         let last_hidden :: Tnsr '[Dirs, t, H] = assert (dirs == 2) $ stack @0 (select @0 @(NumLayers - 1) hidden :. select @0 @NumLayers hidden :. HNil)  -- is it really the last two?
    --         return (last_hidden, spec_)
    -- let f :: (Tnsr '[t, 1, MaxChar], Tnsr '[t, 1, MaxChar])
    --         -> ([Tnsr '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         -> IO ([Tnsr '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tnsr '[t, 1, 2 * Dirs * H] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tnsr '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat
    --                 return (feat : feats, spec_in_, spec_out_)
    -- -- feat_vecs :: [Tnsr '[2 * Dirs * H * t]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- (feat_vecs, _spec_in, _spec_out) :: ([Tnsr '[t, 1, 2 * Dirs * H]], Spec, Spec) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tnsr '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- -- sequential: hidden, loop over t
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tnsr '[1, 1, MaxChar] -> IO (Tnsr '[Dirs, 1, H], Spec) = \ spec tensor -> do
    --         -- tensor :: Tnsr '[1, 1, MaxChar] = UnsafeMkTensor $ select' (toDynamic tensor_in) 0 0  -- asUntyped'
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    --         let (_emb, hidden, cell) :: (
    --                 Tnsr '[1, 1, Dirs * H],
    --                 Tnsr '[Dirs * NumLayers, 1, H],
    --                 Tnsr '[Dirs * NumLayers, 1, H])
    --                     = lstmWithDropout @'BatchFirst lstm tensor
    --         let spec_ :: Spec = LSTMWithConstInitSpec lstm_spec cell hidden
    --         let last_hidden :: Tnsr '[Dirs, 1, H] = assert (dirs == 2) $ stack @0 (select @0 @(Dirs * (NumLayers-1)) hidden :. select @0 @(Dirs * NumLayers - 1) hidden :. HNil)  -- last two?
    --         -- let last_hidden :: Tnsr '[Dirs, 1, H] = assert (dirs == 2) $ stack @0 (select @0 @((Dirs-1) * NumLayers - 1) hidden :. select @0 @(Dirs * NumLayers - 1) hidden :. HNil)  -- last interspersed?
    --         return (last_hidden, spec_)
    -- let f :: (Tnsr '[t, 1, MaxChar], Tnsr '[t, 1, MaxChar])
    --         -> ([Tnsr '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         -> IO ([Tnsr '[t, 1, 2 * Dirs * H]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 -- (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_in, spec_in_) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    --                 select @1 @i
    --                 -- (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- ???
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tnsr '[t, 1, 2 * Dirs * H] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tnsr '[2 * Dirs * H * t] = reshape '[2 * Dirs * H * t] feat
    --                 return (feat : feats, spec_in_, spec_out_)
    -- -- feat_vecs :: [Tnsr '[2 * Dirs * H * t]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- (feat_vecs, _spec_in, _spec_out) :: ([Tnsr '[t, 1, 2 * Dirs * H]], Spec, Spec) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = UnsafeMkTensor $ stack' 0 $ toDynamic <$> feat_vecs
    -- -- let feat_vec :: Tnsr '[batch_size, t, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tnsr '[batch_size, 2 * Dirs * H * t] = asUntyped' (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- pre-vectored
    -- stack input vectors and pad to static dataset size
    let stackPad :: [D.Tensor] -> Tnsr '[batch_size, t, MaxChar] =
            UnsafeMkTensor . F.constantPadNd1d [0, 0, 0, 0, 0, batch_size_ - n_] 0.0 . stack' 0
    let  in_vec :: Tnsr '[batch_size, t, MaxChar] =
            stackPad $ toDynamic . fst <$> vec_pairs
    -- print $ "in_vec: " ++ show (D.shape $ toDynamic in_vec)
    let out_vec :: Tnsr '[batch_size, t, MaxChar] =
            stackPad $ toDynamic . snd <$> vec_pairs
    -- print $ "out_vec: " ++ show (D.shape $ toDynamic out_vec)

    let lstm' = \model -> fstOf3 . lstmWithDropout @'BatchFirst model
    let emb_in  :: Tnsr '[batch_size, t, Dirs * H] = lstm'  in_model  in_vec
    let emb_out :: Tnsr '[batch_size, t, Dirs * H] = lstm' out_model out_vec
    -- print $ "emb_in: " ++ show (D.shape $ toDynamic emb_in)
    -- print $ "emb_out: " ++ show (D.shape $ toDynamic emb_out)

    -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    let feat_vec :: Tnsr '[batch_size, t * (2 * Dirs * H)] =
            reshape $ cat @2 $ emb_in :. emb_out :. HNil
    -- print $ "feat_vec: " ++ show (D.shape $ toDynamic feat_vec)

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

    return feat_vec
