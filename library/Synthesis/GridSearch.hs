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

import System.Log.Logger
import System.ProgressBar
import Control.Applicative
import Control.Exception (finally)
import Control.Monad (forM, join)
import GHC.TypeLits
import GHC.TypeNats (type (+))
import Data.Text.Internal.Lazy (Text)
import Data.Proxy
import Data.Yaml
import Language.Haskell.Interpreter (Interpreter, liftIO)
import Torch.Internal.Managed.Type.Context (manual_seed_L)
import Synthesis.Data
import Synthesis.Hint
import Synthesis.Orphanage ()
import Synthesis.Data (TaskFnDataset (..), SynthesizerConfig (..), EvalResult (..))
import Synthesis.Configs
import Synthesis.Utility
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.NSPS
import Synthesis.Synthesizer.Params

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
    let bestHparComb :: (HparComb, EvalResult) = minBy (lossTest . snd) hparResults
    putStrLn . show $ bestHparComb
    -- TODO: use validation set above, test set here
    res :: (HparComb, EvalResult) <- eval . fst $ bestHparComb
    putStrLn $ "result on test set:\n" <> show res

evalHparComb :: TaskFnDataset -> GridSearchConfig -> HparComb -> IO EvalResult
evalHparComb taskFnDataset cfg hparComb = do
    let cfg' :: SynthesizerConfig = combineConfig cfg hparComb
    let SynthesizerConfig{..} = cfg'
    putStrLn ""
    putStrLn . show $ hparComb
    -- putStrLn . show $ cfg'
    manual_seed_L $ fromIntegral seed
    fmap last . interpretUnsafe $ if False -- hasCuda
        then trainGpu @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @N_train @N_validation @N_test @MaxChar cfg' taskFnDataset
        else trainCpu @M @EncoderBatch @R3nnBatch @Symbols @Rules @MaxStringLength @N_train @N_validation @N_test @MaxChar cfg' taskFnDataset

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

-- import Data.Function.Memoize
-- -- memoize

-- deriveMemoizable ''Double
-- -- deriveMemoizable ''Int
-- deriveMemoizable ''HparComb
