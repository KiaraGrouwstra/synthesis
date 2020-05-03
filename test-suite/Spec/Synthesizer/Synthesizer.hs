{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module Spec.Synthesizer.Synthesizer (module Spec.Synthesizer.Synthesizer) where

import           Test.Tasty                   (TestTree, defaultMain, testGroup)
import           Test.HUnit.Base              (Test (..))
import           Test.HUnit.Text              (runTestTT)
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit             ((@?=))

import           Prelude                      hiding (abs, all)
import           Control.Exception            (SomeException, try, evaluate)
import           Data.Int                     (Int64)
import           Data.Maybe                   (isNothing)
import           Data.Either                  (fromRight, isRight)
import           Data.Functor                 (void, (<&>))
import           Data.Bifunctor               (first, second)
import           Data.HashMap.Lazy            (HashMap, empty, insert, singleton, (!), keys, fromList, toList)
import qualified Data.Set as Set
import           System.Random                (StdGen, mkStdGen)
import           System.Timeout               (timeout)
import           Language.Haskell.Interpreter (as, interpret, liftIO, typeChecks, typeChecksWithDetails)
import           Util                         (fstOf3)
import           Language.Haskell.Interpreter

import           GHC.TypeNats
import           Torch.Typed.Functional
import qualified Torch.Tensor                  as D
import qualified Torch.TensorFactories         as D
import qualified Torch.DType                   as D
import qualified Torch.Optim                   as D
import qualified Torch.Functional.Internal     as I
import qualified Torch.Functional              as F
import qualified Torch.NN                      as A
import           Torch.Typed.Aux
import           Torch.Typed.Tensor
import           Torch.Typed.Factories
import           Torch.Typed.Optim
import           Torch.Typed.Parameter
import           Torch.Typed.NN
import           Torch.Typed.NN.Recurrent.LSTM

import           Synthesis.Ast
import           Synthesis.Configs
import           Synthesis.Blocks
import           Synthesis.FindHoles
import           Synthesis.Generation
import           Synthesis.Hint
import           Synthesis.Types
import           Synthesis.TypeGen
import           Synthesis.Data
import           Synthesis.Utility
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.NSPS
import qualified Synthesis.Synthesizer.Distribution as Distribution
import qualified Synthesis.Synthesizer.Categorical  as Categorical
import           Synthesis.Synthesizer.Params

import           Spec.Synthesizer.Types

type Device = Cpu

synthesizer ∷ Test
synthesizer = let
        dropOut :: Double = 0.0
        dsl = fmap parseExpr
                $ insert "nil" "[]"
                $ insert "not" "not"
                $ singleton "true" "True"
        -- expr_blocks :: [(String, Expr)] <- interpretUnsafe $ dslVariants dsl
        expr_blocks :: [(String, Expr)] = second parseExpr <$> [("nil", "nil"), ("true", "true"), ("not", "not"), ("not", "not (undefined :: Bool)")]
        lr = D.asTensor (0.01 :: Float)
    in TestList

    [ TestLabel "calcLoss" $ TestCase $ do
        let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> expr_blocks
        let variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
        let variantMap :: HashMap String Expr = fromList variants
        let task_fn :: Expr = letIn (pickKeysSafe ["true"] dsl) $ parseExpr "not (not true)"
        taskType :: Tp <- interpretUnsafe $ exprType task_fn
        let symbolIdxs :: HashMap String Int = indexList $ "undefined" : keys dsl
        let io_pairs :: [(Expr, Either String Expr)] = [(parseExpr "0", Right (parseExpr "\"0\"")), (parseExpr "1", Right (parseExpr "\"1\"")), (parseExpr "2", Right (parseExpr "\"2\""))]
        let charMap :: HashMap Char Int = mkCharMap io_pairs
        let encoder_spec :: LstmEncoderSpec Device MaxStringLength EncoderBatch' MaxChar H = LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropOut
        let r3nn_spec :: R3NNSpec Device M Symbols Rules MaxStringLength R3nnBatch' H = initR3nn @M @Symbols @Rules @MaxStringLength @R3nnBatch' @H variants r3nnBatch' dropOut
        model :: NSPS Device M Symbols Rules MaxStringLength EncoderBatch' R3nnBatch' MaxChar H <- A.sample $ NSPSSpec encoder_spec r3nn_spec
        --  :: Tensor Device 'D.Float '[n, 2 * Dirs * H * MaxStringLength]
        io_feats <- lstmEncoder (encoder model) charMap io_pairs
        sampled_feats :: Tensor device 'D.Float '[R3nnBatch', MaxStringLength * (2 * Dirs * H)]
                <- sampleTensor @0 @R3nnBatch' (length io_pairs) $ toDynamic io_feats

        let ruleIdxs :: HashMap String Int = indexList $ fst <$> variants
        let synth_max_holes = 3

        loss :: Tensor Device 'D.Float '[] <- calcLoss dsl task_fn taskType symbolIdxs model sampled_feats variantMap ruleIdxs variant_sizes synth_max_holes
        toFloat loss > 0.0 `shouldBe` True

        let optim :: D.Adam = d_mkAdam 0 0.9 0.999 $ A.flattenParameters model
        (newParam, optim') <- D.runStep model optim (toDynamic loss) lr
        let model' :: NSPS Device M Symbols Rules MaxStringLength EncoderBatch' R3nnBatch' MaxChar H = A.replaceParameters model newParam
        loss' :: Tensor Device 'D.Float '[] <- calcLoss dsl task_fn taskType symbolIdxs model' sampled_feats variantMap ruleIdxs variant_sizes synth_max_holes
        toBool (loss' <. loss) `shouldBe` True

    ]
