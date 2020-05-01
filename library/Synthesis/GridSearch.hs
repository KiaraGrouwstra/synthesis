{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | grid-search logic
module Synthesis.GridSearch (module Synthesis.GridSearch) where

import           System.Log.Logger
import           System.ProgressBar
import           Control.Applicative
import           Control.Exception (finally)
import           Control.Monad (forM, join)
import           GHC.TypeLits
import           GHC.TypeNats (type (+))
import           Data.Text.Internal.Lazy (Text)
import           Data.Proxy
import           Data.Yaml
import           Util (thdOf3)
import           Text.Printf
import           Language.Haskell.Interpreter (Interpreter, liftIO)
import           Torch.Internal.Managed.Type.Context (manual_seed_L)
import           Torch.Typed.Tensor
import           Torch.Typed.NN
import           Torch.Typed.NN.Recurrent.LSTM
import qualified Torch.Tensor                  as D
import qualified Torch.Autograd                as D
import qualified Torch.Serialize               as D
import qualified Torch.NN                      as A
import           Synthesis.Data
import           Synthesis.Hint
import           Synthesis.Orphanage ()
import           Synthesis.Data
import           Synthesis.Configs
import           Synthesis.Utility
import           Synthesis.Hint
import           Synthesis.Synthesizer.Utility
import           Synthesis.Synthesizer.Encoder
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.NSPS
import           Synthesis.Synthesizer.Params

-- | main function
main :: IO ()
main = do
    -- TODO: move all hyper-params here, handle static stuff
    -- print . take 5 $ knownNats @0   -- prints: [0,1,2,3,4]
    cfg :: GridSearchConfig <- parseGridSearchConfig
    let GridSearchConfig{..} = cfg
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    putStrLn . show $ generationCfg
    pb <- newProgressBar pgStyle 1 (Progress 0 (length hparCombs) ("grid-search" :: Text))
    let eval = traverseToSnd $ evalHparComb taskFnDataset cfg
    hparResults :: [(HparComb, EvalResult)] <- forM hparCombs $ (`finally` incProgress pb 1) . eval
    -- TODO: save/plot
    -- could show tie-breakers by `monad-loops`'s `minimaOnM`, but... just visualize.
    let (bestHparComb, bestEvalResult) :: (HparComb, EvalResult) = minBy (lossValid . snd) hparResults
    let HparComb  {..} = bestHparComb
    let EvalResult{..} = bestEvalResult
    printf "Best hyper-parameter combination: %s. Evaluation results: %s.\n" (show bestHparComb) (show bestEvalResult)
    -- finally re-evaluate the chosen hyperparameters on our test set
    manual_seed_L $ fromIntegral seed
    let test_set :: [Expr] = thdOf3 datasets
    let prepped_dsl = prep_dsl taskFnDataset
    let (variants, variant_sizes, task_type_ins, task_io_pairs, task_outputs, symbolIdxs, ruleIdxs, variantMap, max_holes, dsl') = prepped_dsl
    -- hard-coding device to CPU for static typing without code duplication for now
    let encoder_spec :: LstmEncoderSpec Cpu MaxStringLength EncoderBatch MaxChar = LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropoutRate
    let r3nn_spec :: R3NNSpec Cpu M Symbols Rules MaxStringLength R3nnBatch = initR3nn @M @Symbols @Rules @MaxStringLength @R3nnBatch variants r3nnBatch dropoutRate
    -- let device = deviceVal @Cpu
    model :: NSPS Cpu M Symbols Rules MaxStringLength EncoderBatch R3nnBatch MaxChar <- liftIO $ A.sample $ NSPSSpec @Cpu @M @Symbols @Rules encoder_spec r3nn_spec
    let synthCfg :: SynthesizerConfig = combineConfig cfg bestHparComb
    let modelFolder :: String = resultFolder <> "/" <> ppSynCfg synthCfg
    let modelPath :: String = modelFolder <> printf "/%04d.pt" epoch
    params :: [D.Tensor] <- D.load modelPath
    let model' = A.replaceParameters model $ D.IndependentTensor <$> params
    (acc_test, loss_test) <- interpretUnsafe $ evaluate @Cpu @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @MaxChar taskFnDataset prepped_dsl bestOf model' test_set
    printf "Test loss: %.4f. Test accuracy: %.4f.\n" (toFloat loss_test) (toFloat acc_test)

evalHparComb :: TaskFnDataset -> GridSearchConfig -> HparComb -> IO EvalResult
evalHparComb taskFnDataset cfg hparComb = do
    let cfg' :: SynthesizerConfig = combineConfig cfg hparComb
    let SynthesizerConfig{..} = cfg'
    putStrLn ""
    putStrLn . show $ hparComb
    -- putStrLn . show $ cfg'
    manual_seed_L $ fromIntegral seed
    fmap last . interpretUnsafe $ if False -- hasCuda
        then train @Gpu @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @MaxChar cfg' taskFnDataset
        else train @Cpu @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @MaxChar cfg' taskFnDataset

hparCombs :: [HparComb] = uncurry2 HparComb <$> cartesianProduct2
    -- -- performance
    -- dropoutRate :: Double
    -- [0.0, 1.0 :: Double]
    (0 : reverse ((\x -> 2 ** (-x)) <$> [1..5]) :: [Double])
    -- regularization :: Float
    -- [0.0, 0.05 :: Float]
    (0 : reverse ((\x -> 10 ** (-x)) <$> [1..4]) :: [Float])
    -- -- convergence
    -- evalFreq :: Int
    -- checkWindow :: Int
    -- bestOf :: Int
    -- convergenceThreshold :: Float
    -- learningRate :: Float
    -- ((\x -> 10 ** (-x)) <$> [2..4])
    -- learningDecay :: Int
