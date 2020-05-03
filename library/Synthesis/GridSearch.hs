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
import           Control.Monad (mapM, join)
import           GHC.TypeLits
import           GHC.TypeNats (type (+))
import           Data.Text.Internal.Lazy (Text)
import           Data.Proxy
import           Data.Bifunctor (second)
import           Data.Yaml
import           Data.HashMap.Lazy (size)
import           Util (thdOf3)
import           Text.Printf
import           Language.Haskell.Interpreter (Interpreter, liftIO)
import           Torch.Internal.Managed.Type.Context (manual_seed_L)
import           Torch.Typed.Tensor
import           Torch.Typed.Functional
import           Torch.Typed.Factories
import           Torch.Typed.NN
import           Torch.Typed.NN.Recurrent.LSTM
import           Torch.Typed.Aux (natValI)
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
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

hparCombs :: [HparComb] = uncurry6 HparComb <$> cartesianProduct6
    -- dropoutRate :: Double
    (0 : reverse ((\x -> 2 ** (-x)) <$> [1..5]) :: [Double])
    -- regularization :: Float
    (0 : reverse ((\x -> 10 ** (-x)) <$> [1..4]) :: [Float])
    mOpts
    hOpts
    hidden0Opts
    hidden1Opts

-- | skip `m=1`: must be an even number for H.
mOpts :: [Int]
mOpts = (2 ^) <$> [3..7]

hOpts :: [Int]
hOpts = (2 ^) <$> [3..7]

hidden0Opts :: [Int]
hidden0Opts = (2 ^) <$> [2..6]

hidden1Opts :: [Int]
hidden1Opts = (2 ^) <$> [2..6]

-- | main function
main :: IO ()
main = if False -- hasCuda
        then gridSearch @Gpu
        else gridSearch @Cpu

gridSearch :: forall device . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64) => IO ()
gridSearch = do
    cfg :: GridSearchConfig <- parseGridSearchConfig
    let GridSearchConfig{..} = cfg
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    putStrLn . show $ generationCfg
    pb <- newProgressBar pgStyle 1 (Progress 0 (length hparCombs) ("grid-search" :: Text))
    hparResults :: [(HparComb, (EvalResult, IO ()))] <- mapM (`finally` incProgress pb 1) $ (!! length exprBlocks) $ getRules @device @0 cfg taskFnDataset hparCombs

    -- write results to csv
    let resultPath = printf "%s/gridsearch-%s.csv" resultFolder $ ppCfg cfg
    liftIO $ writeCsv resultPath gridSearchHeader $ second fst <$> hparResults
    putStrLn $ "data written to " <> resultPath

    -- could show tie-breakers by `monad-loops`'s `minimaOnM`, but... just visualize.
    snd . minBy (lossValid . fst) $ snd <$> hparResults

-- bending over backward to get the compiler to accept my dynamically calculated values as static

getRules :: forall device rules . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [[IO (HparComb, (EvalResult, IO ()))]]
getRules cfg taskFnDataset hparCombs = (:)
        ((!! (size (charMap taskFnDataset) + 1)) $ getMaxChar @device @rules @0 cfg taskFnDataset hparCombs)
        $ getRules @device @(rules + 1) cfg taskFnDataset hparCombs

getMaxChar :: forall device rules maxChar . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [[IO (HparComb, (EvalResult, IO ()))]]
getMaxChar cfg taskFnDataset hparCombs = (:)
        ((!! (size (dsl taskFnDataset) + natValI @LhsSymbols)) $ getSymbols @device @rules @maxChar @0 cfg taskFnDataset hparCombs)
        $ getMaxChar @device @rules @(maxChar + 1) cfg taskFnDataset hparCombs

getSymbols :: forall device rules maxChar symbols . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar, KnownNat symbols) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [[IO (HparComb, (EvalResult, IO ()))]]
getSymbols cfg taskFnDataset hparCombs = (:)
        ((!! longestString taskFnDataset) $ getMaxStringLength @device @rules @maxChar @symbols @0 cfg taskFnDataset hparCombs)
        $ getSymbols @device @rules @maxChar @(symbols + 1) cfg taskFnDataset hparCombs

getMaxStringLength :: forall device rules maxChar symbols maxStringLength . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [[IO (HparComb, (EvalResult, IO ()))]]
getMaxStringLength cfg taskFnDataset hparCombs = (:)
        (takeAll hOpts $ getH @device @rules @maxChar @symbols @maxStringLength @0 cfg taskFnDataset hparCombs)
        $ getMaxStringLength @device @rules @maxChar @symbols @(maxStringLength + 1) cfg taskFnDataset hparCombs

-- then actually dynamically iterate over hyperparameter values, again to trick the compiler into considering them as static

getH :: forall device rules maxChar symbols maxStringLength h . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [IO (HparComb, (EvalResult, IO ()))]
getH cfg taskFnDataset hparCombs = (<>)
        (takeAll mOpts $ getM @device @rules @maxChar @symbols @maxStringLength @h @0 cfg taskFnDataset hparCombs)
        $ getH @device @rules @maxChar @symbols @maxStringLength @(h + 1) cfg taskFnDataset hparCombs

getM :: forall device rules maxChar symbols maxStringLength h m . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h, KnownNat m) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [IO (HparComb, (EvalResult, IO ()))]
getM cfg taskFnDataset hparCombs = (<>)
        (traverseToSnd (evalHparComb @device @m @rules @maxChar @symbols @maxStringLength @h taskFnDataset cfg) <$> filter (\HparComb{..} -> m == natValI @m) hparCombs)
        $ getM @device @rules @maxChar @symbols @maxStringLength @h @(m + 1) cfg taskFnDataset hparCombs

-- | evaluate a hyper-parameter combination by training a model on them to convergence, returning results plus a button to run final evalution on this
evalHparComb :: forall device m rules maxChar symbols maxStringLength h . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat m, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h) => TaskFnDataset -> GridSearchConfig -> HparComb -> IO (EvalResult, IO ())
evalHparComb taskFnDataset cfg hparComb = do
    let cfg' :: SynthesizerConfig = combineConfig cfg hparComb
    let SynthesizerConfig{..} = cfg'
    putStrLn ""  -- don't touch the progress bar line
    putStrLn . show $ hparComb
    -- putStrLn . show $ cfg'
    manual_seed_L $ fromIntegral seed
    lastEvalResult :: EvalResult <- last <.> interpretUnsafe $ train @device @m @EncoderBatch @R3nnBatch @symbols @rules @maxStringLength @maxChar @h cfg' taskFnDataset
    let testEval :: IO () = finalEval @device @m @rules @maxChar @symbols @maxStringLength @h cfg taskFnDataset hparComb lastEvalResult
    return (lastEvalResult, testEval)

-- | after we finish grid search, do a final evaluation on our test set just in case the grid search overfitted on our validation set
finalEval :: forall device m rules maxChar symbols maxStringLength h . (KnownNat m, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h, KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64) => GridSearchConfig -> TaskFnDataset -> HparComb -> EvalResult -> IO ()
finalEval cfg taskFnDataset bestHparComb bestEvalResult = do
    let GridSearchConfig{..} = cfg
    let TaskFnDataset{..} = taskFnDataset
    let HparComb  {..} = bestHparComb
    let EvalResult{..} = bestEvalResult
    printf "Best hyper-parameter combination: %s\nEvaluation results: %s\n" (show bestHparComb) (show bestEvalResult)
    -- finally re-evaluate the chosen hyperparameters on our test set
    manual_seed_L $ fromIntegral seed
    let test_set :: [Expr] = thdOf3 datasets
    let prepped_dsl = prep_dsl taskFnDataset
    let (variants, variant_sizes, task_type_ins, task_io_pairs, task_outputs, symbolIdxs, ruleIdxs, variantMap, max_holes, dsl') = prepped_dsl
    let encoder_spec :: LstmEncoderSpec device maxStringLength EncoderBatch maxChar h = LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropoutRate
    let r3nn_spec :: R3NNSpec device m symbols rules maxStringLength R3nnBatch h = initR3nn @m @symbols @rules @maxStringLength @R3nnBatch @h variants r3nnBatch dropoutRate hidden0 hidden1
    model :: NSPS device m symbols rules maxStringLength EncoderBatch R3nnBatch maxChar h <- liftIO $ A.sample $ NSPSSpec @device @m @symbols @rules encoder_spec r3nn_spec
    let synthCfg :: SynthesizerConfig = combineConfig cfg bestHparComb
    let modelPath :: String = printf "%s/%s/%04d.pt" resultFolder (ppCfg synthCfg) epoch
    params :: [D.Tensor] <- D.load modelPath
    let model' = A.replaceParameters model $ D.IndependentTensor <$> params
    (acc_test, loss_test) <- interpretUnsafe $ evaluate @device @m @EncoderBatch @R3nnBatch @symbols @rules @maxStringLength @maxChar @h taskFnDataset prepped_dsl bestOf model' test_set
    printf "Test loss: %.4f. Test accuracy: %.4f.\n" (toFloat loss_test) (toFloat acc_test)
