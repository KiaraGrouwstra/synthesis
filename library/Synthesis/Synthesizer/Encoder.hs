{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}

module Synthesis.Synthesizer.Encoder (
    NumLayers,
    Dev,
    H,
    T,
    baseline_lstm_encoder,
) where

-- import GHC.Exts (fromList)
import Data.Bifunctor (first, second)
-- import Control.Exception (assert)
import Data.Int (Int64) 
import Data.Char (ord)
-- import Data.Foldable (foldrM)
-- import qualified GHC.TypeNats
import GHC.TypeNats (Nat, KnownNat, Mod, type (*), type (+), type (-))

import Torch.Typed.Tensor
import Torch.Typed.Functional
import Torch.Typed.NN
-- import Torch.Typed.Factories
import Torch.Typed.Aux
import Torch.HList
-- import qualified Torch.HList
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Functional.Internal     as F
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
-- import qualified Torch.Device                  as D
-- import qualified Torch.Functional.Internal     as D
import Torch.Typed.NN.Recurrent.LSTM
-- import Torch.Functional.Internal (stack)

import Synthesis.Data (Expr)
import Synthesis.Utility (pp) -- , pp_
import Synthesis.Synthesizer.Utility (Dev, Dir, Dirs, asUntyped, padRight, select', rotate) -- rotateT

type NumLayers = 3 -- ?
-- | H is the topmost LSTM hidden dimension
type H = 30 -- ?
-- T is the maximum string length for any input or output string
type T = 20 -- ?  -- TODO: find this dynamically?
-- actually Char seems in Int range, i.e. [-2^29 .. 2^29-1]... I think I wouldn't need more than ascii tho.
type MaxChar = 256
dropoutRate :: Double
dropoutRate = 0.0

type Spec = LSTMWithInitSpec MaxChar H NumLayers Dir 'ConstantInitialization 'D.Float Dev

-- TODO: combine i/o lists across types
-- | 5.1.1 Baseline LSTM encoder
-- | This encoding is conceptually straightforward and has very little prior knowledge about what operations are being performed over the strings, i.e., substring, constant, etc., which might make it difficult to discover substring indices, especially the ones based on regular expressions.

-- TODO: batch data?
baseline_lstm_encoder :: forall n . (KnownNat n) => [(Expr, Either String Expr)] -> IO (Tensor Dev 'D.Float '[n, 2 * Dirs * H * T])
baseline_lstm_encoder io_pairs = do
    -- type vals
    -- let h :: Int = natValI @H
    let t :: Int = natValI @T
    -- let dirs :: Int = natValI @Dirs
    let max_char :: Int = natValI @MaxChar

    -- TODO: use tree encoding (R3NN) also for expressions instead of just converting to string
    let str_pairs :: [(String, String)] = first pp . second (show . second pp) <$> io_pairs  -- second pp_
    -- | Our first I/O encoding network involves running two separate deep bidirectional LSTM networks for processing the input and the output string in each example pair.
    let dropoutSpec :: DropoutSpec = DropoutSpec dropoutRate -- drop-out not mentioned in NSPS
    -- convert char to one-hot encoding (byte -> 256 1/0s as float) as third lstm dimension
    let str2tensor :: Int -> String -> Tensor Dev 'D.Float '[1, T, MaxChar] = \len -> toDType @'D.Float . UnsafeMkTensor . flip F.one_hot max_char . D.asTensor . padRight 0 len . fmap ((fromIntegral :: Int -> Int64) . ord)
    let vec_pairs :: [(Tensor Dev 'D.Float '[1, T, MaxChar], Tensor Dev 'D.Float '[1, T, MaxChar])] = first (str2tensor t) . second (str2tensor t) <$> str_pairs
    -- print $ "vec_pairs: " ++ show (first (show . D.shape . toDynamic) . second (show . D.shape . toDynamic) <$> vec_pairs)
    -- let n_ :: Int = length vec_pairs

    -- -- mapped: I'm not quite sure if this is learning across samples as the lstms seem not updated? should it??
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let spec :: Spec = LSTMWithZerosInitSpec lstm_spec
    -- in_lstm  :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    -- out_lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    -- let feat_vecs :: [Tensor Dev 'D.Float '[2 * Dirs * H * T]] = flip fmap vec_pairs $ \(tensor_in, tensor_out) -> let
    --             (emb_in, hidden_in, cell_in) ::
    --                 (Tensor Dev 'D.Float '[1, T, Dirs * H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, 1, H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, 1, H])
    --                     = lstmWithoutDropout @'BatchFirst in_lstm tensor_in
    --             (emb_out, hidden_out, cell_out) ::
    --                 (Tensor Dev 'D.Float '[1, T, Dirs * H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, 1, H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, 1, H])
    --                     = lstmWithDropout @'BatchFirst out_lstm tensor_out
    --             -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --             -- TODO: check if this remotely makes sense cuz I have no clue
    --             feat :: Tensor Dev 'D.Float '[1, T, 2 * Dirs * H * T] = cat @2 $ emb_in :. emb_out :. HNil
    --             -- flatten results at the end
    --             -- feat_ :: Tensor Dev 'D.Float '[2 * Dirs * H * T] = reshape '[2 * Dirs * H * T] feat
    --         in feat
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = UnsafeMkTensor $ D.stack (toDynamic <$> feat_vecs) 0
    -- -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = UnsafeMkTensor . D.reshape [n_, 2 * Dirs * h * t] . toDynamic $ feat_vec
    -- -- let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = asUntyped (D.reshape [n_, 2 * Dirs * h * t]) feat_vec

    -- -- sequential
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tensor Dev 'D.Float '[1, T, MaxChar] -> IO (Tensor Dev 'D.Float '[1, T, Dirs * H], Spec) = \ spec tensor -> do
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    --         let (emb, hidden, cell) :: (
    --             Tensor Dev 'D.Float '[1, T, Dirs * H],
    --             Tensor Dev 'D.Float '[Dirs * NumLayers, H],
    --             Tensor Dev 'D.Float '[Dirs * NumLayers, H])
    --                 = lstmWithoutDropout @'BatchFirst lstm tensor
    --         let spec_ = LSTMWithConstInitSpec lstm_spec cell hidden
    --         return (emb, spec_)
    -- let f :: (Tensor Dev 'D.Float '[1, T, MaxChar], Tensor Dev 'D.Float '[1, T, MaxChar])
    --         -> ([Tensor Dev 'D.Float '[1, T, 2 * Dirs * H * T]], Spec, Spec)
    --         -> IO ([Tensor Dev 'D.Float '[1, T, 2 * Dirs * H * T]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tensor Dev 'D.Float '[1, T, 2 * Dirs * H * T] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tensor Dev 'D.Float '[2 * Dirs * H * T] = reshape '[2 * Dirs * H * T] feat

    --                 return (feat : feats, spec_in_, spec_out_)
    -- feat_vecs :: [Tensor Dev 'D.Float '[2 * Dirs * H * T]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = UnsafeMkTensor $ D.stack (toDynamic <$> feat_vecs) 0
    -- -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = UnsafeMkTensor . D.reshape [n_, 2 * dirs * h * t] . toDynamic $ feat_vec
    -- -- let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = asUntyped (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- -- sequential: hidden, T as N
    -- let lstm_spec :: LSTMSpec T H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tensor Dev 'D.Float '[T, 1, MaxChar] -> IO (Tensor Dev 'D.Float '[Dirs, T, H], Spec) = \ spec tensor -> do
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    --         let (_emb, hidden, cell) :: (
    --                 Tensor Dev 'D.Float '[T, 1, Dirs * H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, T, H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, T, H])
    --                     = lstmWithoutDropout @'BatchFirst lstm tensor
    --         let spec_ :: Spec = LSTMWithConstInitSpec lstm_spec cell hidden
    --         let last_hidden :: Tensor Dev 'D.Float '[Dirs, T, H] = assert (dirs == 2) $ stack @0 (select @0 @(NumLayers - 1) hidden :. select @0 @NumLayers hidden :. HNil)  -- is it really the last two?
    --         return (last_hidden, spec_)
    -- let f :: (Tensor Dev 'D.Float '[T, 1, MaxChar], Tensor Dev 'D.Float '[T, 1, MaxChar])
    --         -> ([Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H]], Spec, Spec)
    --         -> IO ([Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tensor Dev 'D.Float '[2 * Dirs * H * T] = reshape '[2 * Dirs * H * T] feat
    --                 return (feat : feats, spec_in_, spec_out_)
    -- -- feat_vecs :: [Tensor Dev 'D.Float '[2 * Dirs * H * T]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- (feat_vecs, _spec_in, _spec_out) :: ([Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H]], Spec, Spec) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = UnsafeMkTensor $ D.stack (toDynamic <$> feat_vecs) 0
    -- -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = UnsafeMkTensor . D.reshape [n_, 2 * dirs * h * t] . toDynamic $ feat_vec
    -- -- let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = asUntyped (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- -- sequential: hidden, loop over T
    -- let lstm_spec :: LSTMSpec 1 H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    -- let lstm_step :: Spec -> Tensor Dev 'D.Float '[1, 1, MaxChar] -> IO (Tensor Dev 'D.Float '[Dirs, 1, H], Spec) = \ spec tensor -> do
    --         -- tensor :: Tensor Dev 'D.Float '[1, 1, MaxChar] = UnsafeMkTensor $ select' (toDynamic tensor_in) 0 0  -- asUntyped
    --         lstm :: LSTMWithInit 1 H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
    --         let (_emb, hidden, cell) :: (
    --                 Tensor Dev 'D.Float '[1, 1, Dirs * H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, 1, H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, 1, H])
    --                     = lstmWithoutDropout @'BatchFirst lstm tensor
    --         let spec_ :: Spec = LSTMWithConstInitSpec lstm_spec cell hidden
    --         let last_hidden :: Tensor Dev 'D.Float '[Dirs, 1, H] = assert (dirs == 2) $ stack @0 (select @0 @(Dirs * (NumLayers-1)) hidden :. select @0 @(Dirs * NumLayers - 1) hidden :. HNil)  -- last two?
    --         -- let last_hidden :: Tensor Dev 'D.Float '[Dirs, 1, H] = assert (dirs == 2) $ stack @0 (select @0 @((Dirs-1) * NumLayers - 1) hidden :. select @0 @(Dirs * NumLayers - 1) hidden :. HNil)  -- last interspersed?
    --         return (last_hidden, spec_)
    -- let f :: (Tensor Dev 'D.Float '[T, 1, MaxChar], Tensor Dev 'D.Float '[T, 1, MaxChar])
    --         -> ([Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H]], Spec, Spec)
    --         -> IO ([Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H]], Spec, Spec)
    --         = \ (tensor_in, tensor_out) (feats, spec_in, spec_out) -> do
    --                 -- (emb_in, spec_in_) <- lstm_step spec_in tensor_in
    --                 (emb_in, spec_in_) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    --                 select @1 @i
    --                 -- (emb_out, spec_out_) <- lstm_step spec_out tensor_out
    --                 -- ???
    --                 -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    --                 -- TODO: check if this remotely makes sense cuz I have no clue
    --                 let feat :: Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H] = cat @2 $ emb_in :. emb_out :. HNil
    --                 -- flatten results at the end
    --                 -- feat_ :: Tensor Dev 'D.Float '[2 * Dirs * H * T] = reshape '[2 * Dirs * H * T] feat
    --                 return (feat : feats, spec_in_, spec_out_)
    -- -- feat_vecs :: [Tensor Dev 'D.Float '[2 * Dirs * H * T]] <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- (feat_vecs, _spec_in, _spec_out) :: ([Tensor Dev 'D.Float '[T, 1, 2 * Dirs * H]], Spec, Spec) <- foldrM f ([], LSTMWithZerosInitSpec lstm_spec, LSTMWithZerosInitSpec lstm_spec) vec_pairs
    -- -- | We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = UnsafeMkTensor $ D.stack (toDynamic <$> feat_vecs) 0
    -- -- let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = stack @0 feat_vecs
    -- -- flatten results at the end
    -- let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = asUntyped (D.reshape [n_, 2 * dirs * h * t]) feat_vec

    -- pre-vectored
    let lstm_spec :: LSTMSpec MaxChar H NumLayers Dir 'D.Float Dev = LSTMSpec dropoutSpec
    let spec :: Spec = LSTMWithZerosInitSpec lstm_spec
    -- stack input vectors, dynamic cuz no static dataset size
    let  in_vec :: Tensor Dev 'D.Float '[n, T, MaxChar] = UnsafeMkTensor $ F.stack (fmap (toDynamic . fst) vec_pairs) 0
    -- print $ "in_vec: " ++ show (D.shape $ toDynamic in_vec)
    let out_vec :: Tensor Dev 'D.Float '[n, T, MaxChar] = UnsafeMkTensor $ F.stack (fmap (toDynamic . snd) vec_pairs) 0
    -- print $ "out_vec: " ++ show (D.shape $ toDynamic out_vec)
    -- TODO: replace this with an untyped lstm
    let lstm_step :: Tensor Dev 'D.Float '[n, T, MaxChar] -> IO (Tensor Dev 'D.Float '[n, T, Dirs * H]) = \tensor -> do
            lstm_model :: LSTMWithInit MaxChar H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample spec
            let (emb, _hidden, _cell) :: (
                    Tensor Dev 'D.Float '[n, T, Dirs * H],
                    Tensor Dev 'D.Float '[Dirs * NumLayers, n, H],
                    Tensor Dev 'D.Float '[Dirs * NumLayers, n, H])
                        = lstmWithoutDropout @'BatchFirst lstm_model tensor
            return emb
    emb_in  <- lstm_step  in_vec
    -- print $ "emb_in: " ++ show (D.shape $ toDynamic emb_in)
    emb_out <- lstm_step out_vec
    -- print $ "emb_out: " ++ show (D.shape $ toDynamic emb_out)

    -- | For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair
    let feat_vec :: Tensor Dev 'D.Float '[n, T, 2 * Dirs * H] = cat @2 $ emb_in :. emb_out :. HNil
    let feat_vec_ :: Tensor Dev 'D.Float '[n, 2 * Dirs * H * T] = reshape feat_vec

    -- | 5.1.2 Cross Correlation encoder

    -- | To help the model discover input substrings that are copied to the output, we designed an novel I/O example encoder to compute the cross correlation between each input and output example representation.
    -- | We used the two output tensors of the LSTM encoder (discussed above) as inputs to this encoder.
    -- | For each example pair, we first slide the output feature block over the input feature block and compute the dot product between the respective position representation.
    -- | Then, we sum over all overlapping time steps.
    -- | Features of all pairs are then concatenated to form a 2∗(T−1)-dimensional vector encoding for all example pairs.
    -- | There are 2∗(T−1) possible alignments in total between input and output feature blocks.
    -- | We also designed the following variants of this encoder.

    -- Cross Correlation encoder
    -- h1_in, h1_out
    -- dot(a, b)
    -- mm(a, b)
    -- matmul(a, b) performs matrix multiplications if both arguments are 2D and computes their dot product if both arguments are 1D
    -- bmm(a, b)
    -- bdot(a, b): (a*b).sum(-1)  -- https://github.com/pytorch/pytorch/issues/18027

    -- | Diffused Cross Correlation Encoder:
    -- | This encoder is identical to the Cross Correlation encoder except that instead of summing over overlapping time steps after the element-wise dot product, we simply concatenate the vectors corresponding to all time steps, resulting in a final representation that contains 2∗(T−1)∗T features for each example pair.

    -- | LSTM-Sum Cross Correlation Encoder:
    -- | In this variant of the Cross Correlation encoder, instead of doing an element-wise dot product, we run a bidirectional LSTM over the concatenated feature blocks of each alignment.
    -- | We represent each alignment by the LSTM hidden representation of the final time step leading to a total of 2∗H∗2∗(T−1) features for each example pair.

    -- | Augmented Diffused Cross Correlation Encoder:
    -- | For this encoder, the output of each character position of the Diffused Cross Correlation encoder is combined with the character embedding at this position, then a basic LSTM encoder is run over the combined features to extract a 4∗H-dimensional vector for both the input and the output streams.
    -- | The LSTM encoder output is then concatenated with the output of the Diffused Cross Correlation encoder forming a (4∗H+T∗(T−1))-dimensional feature vector for each example pair.

    return feat_vec_
