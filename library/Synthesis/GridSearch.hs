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

-- | main function
main :: IO ()
main = if False -- hasCuda
        then gridSearch @Gpu
        else gridSearch @Cpu

gridSearch :: forall device . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64) => IO ()
gridSearch = do
    -- TODO: move all hyper-params here
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
    let (_bestEvalResult, testEval) :: (EvalResult, IO ()) = minBy (lossValid . fst) $ snd <$> hparResults
    testEval

getRules :: forall device rules . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat rules) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [[IO (HparComb, (EvalResult, IO ()))]]
getRules cfg taskFnDataset hparCombs = (:)
        (takeAll mOpts $ getM @device @0 @rules cfg taskFnDataset hparCombs)
        $ getRules @device @(rules + 1) cfg taskFnDataset hparCombs

getM :: forall device m rules . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat m, KnownNat rules) => GridSearchConfig -> TaskFnDataset -> [HparComb] -> [IO (HparComb, (EvalResult, IO ()))]
getM cfg taskFnDataset hparCombs = (<>)
        (traverseToSnd (evalHparComb @device @m @rules taskFnDataset cfg) <$> filter ((== natValI @m) . m) hparCombs)
        $ getM @device @(m + 1) @rules cfg taskFnDataset hparCombs

finalEval :: forall device m rules . (KnownNat m, KnownNat rules, KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64) => GridSearchConfig -> TaskFnDataset -> HparComb -> EvalResult -> IO ()
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
    let encoder_spec :: LstmEncoderSpec device MaxStringLength EncoderBatch MaxChar = LstmEncoderSpec $ LSTMSpec $ DropoutSpec dropoutRate
    let r3nn_spec :: R3NNSpec device m Symbols rules MaxStringLength R3nnBatch = initR3nn @m @Symbols @rules @MaxStringLength @R3nnBatch variants r3nnBatch dropoutRate
    model :: NSPS device m Symbols rules MaxStringLength EncoderBatch R3nnBatch MaxChar <- liftIO $ A.sample $ NSPSSpec @device @m @Symbols @rules encoder_spec r3nn_spec
    let synthCfg :: SynthesizerConfig = combineConfig cfg bestHparComb
    let modelPath :: String = printf "%s/%s/%04d.pt" resultFolder (ppCfg synthCfg) epoch
    params :: [D.Tensor] <- D.load modelPath
    let model' = A.replaceParameters model $ D.IndependentTensor <$> params
    (acc_test, loss_test) <- interpretUnsafe $ evaluate @device @m @EncoderBatch @R3nnBatch @Symbols @rules @MaxStringLength @MaxChar taskFnDataset prepped_dsl bestOf model' test_set
    printf "Test loss: %.4f. Test accuracy: %.4f.\n" (toFloat loss_test) (toFloat acc_test)

evalHparComb :: forall device m rules . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64, KnownNat m, KnownNat rules) => TaskFnDataset -> GridSearchConfig -> HparComb -> IO (EvalResult, IO ())
evalHparComb taskFnDataset cfg hparComb = do
    let cfg' :: SynthesizerConfig = combineConfig cfg hparComb
    let SynthesizerConfig{..} = cfg'
    putStrLn ""  -- don't touch the progress bar line
    putStrLn . show $ hparComb
    -- putStrLn . show $ cfg'
    manual_seed_L $ fromIntegral seed
    lastEvalResult :: EvalResult <- last <.> interpretUnsafe $ train @device @m @EncoderBatch @R3nnBatch @Symbols @rules @MaxStringLength @MaxChar cfg' taskFnDataset
    let testEval :: IO () = finalEval @device @m @rules cfg taskFnDataset hparComb lastEvalResult
    return (lastEvalResult, testEval)

-- | get the finite part of an infinite list including any natural number in the original list
takeAll :: Foldable t => t Int -> [a] -> [a]
takeAll = take . maximum

hparCombs :: [HparComb] = uncurry3 HparComb <$> cartesianProduct3
    -- dropoutRate :: Double
    (0 : reverse ((\x -> 2 ** (-x)) <$> [1..5]) :: [Double])
    -- regularization :: Float
    (0 : reverse ((\x -> 10 ** (-x)) <$> [1..4]) :: [Float])
    mOpts

-- | skip `m=1`: must be an even number for H.
mOpts :: [Int]
mOpts = (2 ^) <$> [3..7]
