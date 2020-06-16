{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | grid-search logic
module Synthesis.GridSearch (module Synthesis.GridSearch) where

import           System.Log.Logger
import           System.ProgressBar
import           System.Random (StdGen, mkStdGen)
import           Control.Applicative
import           Control.Exception (finally)
import           Control.Monad (mapM, join)
import           GHC.TypeLits
import           GHC.TypeNats (type (+))
import           Data.Text.Internal.Lazy (Text)
import           Data.Proxy
import           Data.Bifunctor (second)
import           Data.Yaml
import           Data.HashMap.Lazy (HashMap, size, fromList)
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
import           Synthesis.Synthesizer.TypeEncoder
import           Synthesis.Synthesizer.R3NN
import           Synthesis.Synthesizer.Synthesizer
import           Synthesis.Synthesizer.NSPS
import           Synthesis.Synthesizer.Train
import           Synthesis.Synthesizer.Params

hparCombs :: [HparComb] = uncurry5 HparComb <$> cartesianProduct5
    learningRateOpts
    dropoutRateOpts
    regularizationOpts
    mOpts
    hOpts

learningRateOpts :: [Float]
learningRateOpts = reverse ((\x -> 10 ** (-x)) <$> [3..6])

dropoutRateOpts :: [Double]
dropoutRateOpts = [dropoutRateDef] -- 0 : reverse ((\x -> 2 ** (-x)) <$> [1..5])

regularizationOpts :: [Float]
regularizationOpts = [regularizationDef] -- 0 : reverse ((\x -> 10 ** (-x)) <$> [1..4])

-- | skip `m=1`: must be an even number for H.
mOpts :: [Int]
mOpts = [mDef] -- (2 ^) <$> [3..7]

hOpts :: [Int]
hOpts = [hDef] -- (2 ^) <$> [3..7]

-- | main function
main :: IO ()
main = if False -- hasCuda
        then gridSearch @Gpu
        else gridSearch @Cpu

gridSearch :: forall device . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64) => IO ()
gridSearch = do
    gridCfg :: GridSearchConfig <- parseGridSearchConfig
    let GridSearchConfig{..} = gridCfg
    let cfg = OptimizationConfig{..}
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    putStrLn . show $ generationCfg
    pb <- newProgressBar pgStyle 1 (Progress 0 (length hparCombs) ("grid-search" :: Text))
    let stdGen :: StdGen = mkStdGen seed
    let hparCombs' :: [(HparComb, IO (EvalResult, IO ()))] =
            fmap (second (`finally` incProgress pb 1)) $ join . join $ (!! length exprBlocks) $
            -- featMult
            if useTypes then
                getRules @device @2 @0 cfg taskFnDataset hparCombs
            else
                getRules @device @1 @0 cfg taskFnDataset hparCombs
    let (hparCombs'', _gen') = fisherYates stdGen hparCombs'    -- shuffle
    hparResults :: [(HparComb, (EvalResult, IO ()))] <- sequence $ traverseSnd <$> take evalRounds hparCombs''

    -- write results to csv
    let resultPath = printf "%s/gridsearch-%s.csv" resultFolder $ ppCfg cfg
    liftIO $ writeCsv resultPath gridSearchHeader $ second fst <$> hparResults
    putStrLn $ "data written to " <> resultPath

    -- could show tie-breakers by `monad-loops`'s `minimaOnM`, but... just visualize.
    snd . minBy (lossValid . fst) $ snd <$> hparResults

-- bending over backward to get the compiler to accept my dynamically calculated values as static

getRules :: forall device featMult rules . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules) => OptimizationConfig -> TaskFnDataset -> [HparComb] -> [[[[(HparComb, IO (EvalResult, IO ()))]]]]
getRules cfg taskFnDataset hparCombs = let
    TaskFnDataset{..} = taskFnDataset
    useTypes = natValI @featMult > 1
    charMap = if useTypes then bothCharMap else exprCharMap
    in (:)
        ((!! (size charMap + 1)) $ getMaxChar @device @featMult @rules @0 cfg taskFnDataset hparCombs)
        $ getRules @device @featMult @(rules + 1) cfg taskFnDataset hparCombs

getMaxChar :: forall device featMult rules maxChar . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar) => OptimizationConfig -> TaskFnDataset -> [HparComb] -> [[[[(HparComb, IO (EvalResult, IO ()))]]]]
getMaxChar cfg taskFnDataset hparCombs = (:)
        ((!! (size (dsl taskFnDataset) + natValI @LhsSymbols)) $ getSymbols @device @featMult @rules @maxChar @0 cfg taskFnDataset hparCombs)
        $ getMaxChar @device @featMult @rules @(maxChar + 1) cfg taskFnDataset hparCombs

getSymbols :: forall device featMult rules maxChar symbols . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar, KnownNat symbols) => OptimizationConfig -> TaskFnDataset -> [HparComb] -> [[[[(HparComb, IO (EvalResult, IO ()))]]]]
getSymbols cfg taskFnDataset hparCombs = let
        useTypes = natValI @featMult > 1
        longest = if useTypes then longestString else longestExprString
    in (:)
        ((!! longest taskFnDataset) $ getMaxStringLength @device @featMult @rules @maxChar @symbols @0 cfg taskFnDataset hparCombs)
        $ getSymbols @device @featMult @rules @maxChar @(symbols + 1) cfg taskFnDataset hparCombs

getMaxStringLength :: forall device featMult rules maxChar symbols maxStringLength . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength) => OptimizationConfig -> TaskFnDataset -> [HparComb] -> [[[[(HparComb, IO (EvalResult, IO ()))]]]]
getMaxStringLength cfg taskFnDataset hparCombs = (:)
        (pickIdxs hOpts $ getH @device @featMult @rules @maxChar @symbols @maxStringLength @0 cfg taskFnDataset hparCombs)
        $ getMaxStringLength @device @featMult @rules @maxChar @symbols @(maxStringLength + 1) cfg taskFnDataset hparCombs

-- then actually dynamically iterate over hyperparameter values, again to trick the compiler into considering them as static

getH :: forall device featMult rules maxChar symbols maxStringLength h . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h) => OptimizationConfig -> TaskFnDataset -> [HparComb] -> [[[(HparComb, IO (EvalResult, IO ()))]]]
getH cfg taskFnDataset hparCombs = (:)
        (pickIdxs mOpts $ getM @device @featMult @rules @maxChar @symbols @maxStringLength @h @0 cfg taskFnDataset hparCombs)
        $ getH @device @featMult @rules @maxChar @symbols @maxStringLength @(h + 1) cfg taskFnDataset hparCombs

getM :: forall device featMult rules maxChar symbols maxStringLength h m . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h, KnownNat m) => OptimizationConfig -> TaskFnDataset -> [HparComb] -> [[(HparComb, IO (EvalResult, IO ()))]]
getM cfg taskFnDataset hparCombs = (:)
        (mapToSnd (evalHparComb @device @featMult @m @rules @maxChar @symbols @maxStringLength @h taskFnDataset cfg) <$> filter (\HparComb{..} -> m == natValI @m) hparCombs)
        $ getM @device @featMult @rules @maxChar @symbols @maxStringLength @h @(m + 1) cfg taskFnDataset hparCombs

-- | evaluate a hyper-parameter combination by training a model on them to convergence, returning results plus a button to run final evalution on this
evalHparComb :: forall device featMult m rules maxChar symbols maxStringLength h shape synthesizer ruleFeats . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, KnownNat m, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h, KnownNat ruleFeats, KnownShape shape, shape ~ '[R3nnBatch, maxStringLength * (2 * featMult * Dirs * h)], synthesizer ~ NSPS device m symbols rules maxStringLength EncoderBatch R3nnBatch maxChar h featMult, Synthesizer device shape rules ruleFeats synthesizer, ruleFeats ~ (maxStringLength * m)) => TaskFnDataset -> OptimizationConfig -> HparComb -> IO (EvalResult, IO ())
evalHparComb taskFnDataset cfg hparComb = do
    let cfg' :: SynthesizerConfig = combineConfig cfg hparComb
    let SynthesizerConfig{..} = cfg'
    let TaskFnDataset{..} = taskFnDataset
    let variants :: [(String, Expr)] = (\(_k, v) -> (nodeRule v, v)) <$> exprBlocks
    putStrLn ""  -- don't touch the progress bar line
    putStrLn . show $ hparComb
    -- putStrLn . show $ cfg'
    manual_seed_L $ fromIntegral seed
    model :: NSPS device m symbols rules maxStringLength EncoderBatch R3nnBatch maxChar h featMult
            <- A.sample $ nspsSpec taskFnDataset variants r3nnBatch dropoutRate
    lastEvalResult :: EvalResult <- last <.> interpretUnsafe $ train @device @rules @shape @ruleFeats cfg' taskFnDataset model
    let testEval :: IO () = finalEval @device @featMult @m @rules @maxChar @symbols @maxStringLength @h @shape @ruleFeats cfg taskFnDataset hparComb lastEvalResult
    return (lastEvalResult, testEval)

-- | after we finish grid search, do a final evaluation on our test set just in case the grid search overfitted on our validation set
finalEval :: forall device featMult m rules maxChar symbols maxStringLength h shape ruleFeats . (KnownNat m, KnownNat rules, KnownNat maxChar, KnownNat symbols, KnownNat maxStringLength, KnownNat h, KnownNat ruleFeats, KnownShape shape, shape ~ '[R3nnBatch, maxStringLength * (2 * featMult * Dirs * h)], KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat featMult, ruleFeats ~ (maxStringLength * m)) => OptimizationConfig -> TaskFnDataset -> HparComb -> EvalResult -> IO ()
finalEval cfg taskFnDataset bestHparComb bestEvalResult = do
    let OptimizationConfig{..} = cfg
    let TaskFnDataset{..} = taskFnDataset
    let HparComb  {..} = bestHparComb
    let EvalResult{..} = bestEvalResult
    printf "Best hyper-parameter combination: %s\nEvaluation results: %s\n" (show bestHparComb) (show bestEvalResult)
    -- finally re-evaluate the chosen hyperparameters on our test set
    manual_seed_L $ fromIntegral seed
    let test_set :: [Expr] = thdOf3 datasets
    prepped_dsl <- interpretUnsafe $ prep_dsl taskFnDataset
    let PreppedDSL{..} = prepped_dsl
    let useTypes = natValI @featMult > 1
    let charMap = if useTypes then bothCharMap else exprCharMap
    let encoder_spec :: LstmEncoderSpec device maxStringLength EncoderBatch maxChar h featMult =
            LstmEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropoutRate
    let r3nn_spec :: R3NNSpec device m symbols rules maxStringLength R3nnBatch h maxChar featMult =
            initR3nn variants r3nnBatch dropoutRate charMap
    let rule_encoder_spec :: TypeEncoderSpec device maxStringLength maxChar m =
            TypeEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropoutRate
    model :: NSPS device m symbols rules maxStringLength EncoderBatch R3nnBatch maxChar h featMult <-
            liftIO $ A.sample $ NSPSSpec encoder_spec rule_encoder_spec r3nn_spec
    let synthCfg :: SynthesizerConfig = combineConfig cfg bestHparComb
    let modelPath :: String = printf "%s/%s/%04d.pt" resultFolder (ppCfg synthCfg) epoch
    params :: [D.Tensor] <- D.load modelPath
    let model' = A.replaceParameters model $ D.IndependentTensor <$> params
    (acc_test, loss_test) <- interpretUnsafe $ evaluate @device @rules @shape @ruleFeats taskFnDataset prepped_dsl bestOf maskBad model' test_set
    printf "Test loss: %.4f. Test accuracy: %.4f.\n" (toFloat loss_test) (toFloat acc_test)
