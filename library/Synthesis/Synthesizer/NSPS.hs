{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.NSPS (module Synthesis.Synthesizer.NSPS) where

import           System.Random                 (StdGen, mkStdGen)
import           System.Timeout                (timeout)
import           System.Directory              (createDirectoryIfMissing)
import           System.CPUTime
import           Data.Foldable                 (foldrM)
import           Data.Maybe                    (fromMaybe)
import           Data.Set                      (Set, empty, insert)
import qualified Data.Set
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.HashMap.Lazy             (HashMap, (!), elems, keys, size, mapWithKey, filterWithKey)
import qualified Data.Csv as Csv
import           Data.Text.Prettyprint.Doc (pretty)
import           Text.Printf
import           Foreign.Marshal.Utils         (fromBool)
import           Control.Monad                 (join, replicateM, forM, void, when)
import           Language.Haskell.Exts.Syntax  ( Exp (..) )
import           Prelude                        hiding (abs)
import           Language.Haskell.Interpreter  ( Interpreter, liftIO, lift )
import           GHC.Exts
import           GHC.Generics                  (Generic)
import           GHC.TypeNats                  (KnownNat, Nat, type (*), type (-))
import qualified Torch.Functional.Internal     as I
import qualified Torch.DType                   as D
import qualified Torch.Tensor                  as D
import qualified Torch.Device                  as D
import qualified Torch.TensorFactories         as D
import qualified Torch.TensorOptions           as D
import qualified Torch.Optim                   as D
import qualified Torch.Serialize               as D
import qualified Torch.Autograd                as D
import qualified Torch.Functional              as F
import qualified Torch.NN                      as A
import           Torch.Typed.NN.Recurrent.LSTM
import           Torch.Typed.Aux
import           Torch.Typed.Tensor
import           Torch.Typed.NN
import           Torch.Typed.Parameter
import qualified Torch.Typed.Parameter
import           Torch.Typed.Factories
import           Torch.Typed.Optim
import           Torch.Typed.Functional
import           Torch.Typed.Autograd
import           Torch.Typed.Serialize
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical as Categorical

import           Synthesis.Orphanage ()
import           Synthesis.Data hiding (GridSearchConfig(..), EvolutionaryConfig(..))
import           Synthesis.Utility
import           Synthesis.Ast
import           Synthesis.Generation
import           Synthesis.FindHoles
import           Synthesis.Hint
import           Synthesis.Types
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import           Synthesis.Synthesizer.TypeEncoder
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.Params
import           Synthesis.Synthesizer.Synthesizer

instance ( KnownDevice device, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat m, KnownNat symbols, KnownNat rules, KnownNat maxStringLength, KnownNat encoderBatch, KnownNat r3nnBatch, KnownNat maxChar, KnownNat h, KnownNat featMult, shape ~ '[r3nnBatch, maxStringLength * (2 * featMult * Dirs * h)], ruleFeats ~ (maxStringLength * m) ) => Synthesizer device shape rules ruleFeats (NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult) where

    encode :: NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
            -> HashMap (Tp, Tp) [(Expr, Either String Expr)]
            -> IO (Tensor device 'D.Float shape)
    -- | sampling embedded features to guarantee a static output size
    -- | to allow R3NN a fixed number of samples for its LSTMs, I'm sampling the actual features to make up for potentially multiple type instances giving me a variable number of i/o samples.
    -- | I opted to pick sampling with replacement, which both more naturally handles sample sizes exceeding the number of items, while also seeming to match the spirit of mini-batching by providing more stochastic gradients.
    -- | for my purposes, being forced to pick a fixed sample size means simpler programs with few types may potentially be learned more easily than programs with e.g. a greater number of type instances.
    -- | there should be fancy ways to address this like giving more weight to hard programs (/ samples).
    -- sampled_feats :: Tensor device 'D.Float '[r3nnBatch, maxStringLength * (2 * featMult * Dirs * h)]
    encode mdl io_pairs = sampleTensor @0 @r3nnBatch (length io_pairs) . toDynamic $ lstmEncoder @encoderBatch @maxStringLength @maxChar @r3nnBatch @device @h (encoder mdl) io_pairs

    rule_encode :: NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
                -> [Tp]
                -> Tensor device 'D.Float '[rules, ruleFeats]
    rule_encode mdl types = typeEncoder @rules (rule_encoder mdl) types

    predict   :: forall num_holes
                 . NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
                -> HashMap String Int
                -> Expr
                -> Tensor device 'D.Float '[rules, ruleFeats]
                -> Tensor device 'D.Float shape
                -> Tensor device 'D.Float '[num_holes, rules]
    predict mdl = runR3nn $ r3nn (mdl :: NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult)

    patchLoss :: NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
                -> HashMap String Int
                -> Tensor device 'D.Float '[]
                -> Tensor device 'D.Float '[]
    patchLoss = patchR3nnLoss . r3nn

nspsSpec :: forall device m symbols maxStringLength encoderBatch r3nnBatch maxChar h rules featMult . (KnownNat rules, KnownNat m, KnownNat symbols, KnownNat rules, KnownNat maxStringLength, KnownNat encoderBatch, KnownNat r3nnBatch, KnownNat maxChar, KnownNat h, KnownNat featMult) => TaskFnDataset -> [(String, Expr)] -> Int -> Double -> Int -> Int -> NSPSSpec device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
nspsSpec TaskFnDataset{..} variants r3nnBatch dropoutRate = spec where
    useTypes = natValI @featMult > 1
    charMap = if useTypes then bothCharMap else exprCharMap
    encoder_spec :: LstmEncoderSpec device maxStringLength encoderBatch maxChar h featMult =
        LstmEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropoutRate
    type_encoder_spec :: TypeEncoderSpec device maxStringLength maxChar m =
        TypeEncoderSpec ruleCharMap $ LSTMSpec $ DropoutSpec dropoutRate
    r3nn_spec :: R3NNSpec device m symbols rules maxStringLength r3nnBatch h maxChar featMult =
        initR3nn variants r3nnBatch dropoutRate ruleCharMap
    spec :: NSPSSpec device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult =
        NSPSSpec encoder_spec type_encoder_spec r3nn_spec

data NSPSSpec (device :: (D.DeviceType, Nat)) (m :: Nat) (symbols :: Nat) (rules :: Nat) (maxStringLength :: Nat) (encoderBatch :: Nat) (r3nnBatch :: Nat) (maxChar :: Nat) (h :: Nat) (featMult :: Nat) where
  NSPSSpec :: forall device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
     . { encoderSpec :: LstmEncoderSpec device maxStringLength encoderBatch maxChar h featMult
       , typeEncoderSpec :: TypeEncoderSpec device maxStringLength maxChar m
       , r3nnSpec :: R3NNSpec device m symbols rules maxStringLength r3nnBatch h maxChar featMult }
    -> NSPSSpec device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
 deriving (Show)

data NSPS (device :: (D.DeviceType, Nat)) (m :: Nat) (symbols :: Nat) (rules :: Nat) (maxStringLength :: Nat) (encoderBatch :: Nat) (r3nnBatch :: Nat) (maxChar :: Nat) (h :: Nat) (featMult :: Nat) where
  NSPS :: forall device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
        . { encoder :: LstmEncoder device maxStringLength encoderBatch maxChar h featMult
          , rule_encoder :: TypeEncoder device maxStringLength maxChar m
          , r3nn :: R3NN device m symbols rules maxStringLength r3nnBatch h maxChar featMult }
       -> NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult
 deriving (Show, Generic)

instance ( KnownNat m, KnownNat symbols, KnownNat rules, KnownNat maxStringLength, KnownNat encoderBatch, KnownNat r3nnBatch, KnownNat maxChar, KnownNat h, KnownNat featMult )
  => A.Parameterized (NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult)

instance ( KnownDevice device, RandDTypeIsValid device 'D.Float, KnownNat m, KnownNat symbols, KnownNat rules, KnownNat maxStringLength, KnownNat encoderBatch, KnownNat r3nnBatch, KnownNat maxChar, KnownNat h, KnownNat featMult )
  => A.Randomizable (NSPSSpec device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult) (NSPS device m symbols rules maxStringLength encoderBatch r3nnBatch maxChar h featMult) where
    sample NSPSSpec {..} = NSPS
            <$> A.sample encoderSpec
            <*> A.sample typeEncoderSpec
            <*> A.sample r3nnSpec
