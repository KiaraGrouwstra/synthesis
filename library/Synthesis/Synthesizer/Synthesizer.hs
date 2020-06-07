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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.Synthesizer (module Synthesis.Synthesizer.Synthesizer) where

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
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.Params

class (KnownDevice device, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, A.Parameterized synthesizer) => Synthesizer device shape rules ruleFeats synthesizer where
    encode    :: synthesizer
                -> HashMap (Tp, Tp) [(Expr, Either String Expr)]
                -> IO (Tensor device 'D.Float shape)
    rule_encode :: synthesizer
                -> [Tp]
                -> Tensor device 'D.Float '[rules, ruleFeats]
    predict   :: forall num_holes
                 . synthesizer
                -> HashMap String Int
                -> Expr
                -> Tensor device 'D.Float '[rules, ruleFeats]
                -> Tensor device 'D.Float shape
                -> Tensor device 'D.Float '[num_holes, rules]
    patchLoss ::   synthesizer
                -> HashMap String Int
                -> Tensor device 'D.Float '[]
                -> Tensor device 'D.Float '[]
    patchLoss _model _variant_sizes = id
    doStep    :: forall optimizer . (D.Optimizer optimizer)
                => synthesizer
                -> optimizer
                -> Tensor device 'D.Float '[]
                -> Tensor device 'D.Float '[]
                -> IO ([A.Parameter], optimizer)
    doStep model optim loss lr = D.runStep model optim (toDynamic loss) $ toDynamic lr
