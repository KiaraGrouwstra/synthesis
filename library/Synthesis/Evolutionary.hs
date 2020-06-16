{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
{-# LANGUAGE GADTs #-}

module Synthesis.Evolutionary (module Synthesis.Evolutionary) where

import           System.Random
import           System.IO.Memoize
import           GA (Entity(..), GAConfig(..), evolveVerbose, randomSearch)
-- import Control.Lens
import           Data.Text.Internal.Lazy (Text)
import           Data.HashMap.Lazy (HashMap, fromList, (!), size, keys)
import           Data.Bifunctor (second)
import           Data.Hashable (Hashable (..))
import           Data.Yaml
import           Text.Printf
import           Control.Exception (finally)
import           Control.Monad (join)

import           Torch.Internal.Managed.Type.Context (manual_seed_L)
import           Torch.Typed.Tensor
import           Torch.Typed.Functional hiding (sqrt, round)
import           Torch.Typed.Factories
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
import qualified Torch.Autograd                as D
import qualified Torch.Serialize               as D
import qualified Torch.NN                      as A

import Synthesis.Data hiding (SynthesizerConfig(..))
import Synthesis.Configs
import Synthesis.Utility
import Synthesis.Synthesizer.Utility
import Synthesis.GridSearch hiding (main)

clamp' :: Ord a => a -> a -> a -> a
clamp' lo hi = Prelude.max lo . Prelude.min hi

class Variable variable a where
    genRand :: Int -> variable -> a
    mutate  :: Int -> variable -> a -> a

-- a list of categorical options
data Categorical a where
    Categorical :: [a] -> Categorical a
instance Variable (Categorical a) a where
    genRand seed (Categorical opts) = pickG seed opts
    mutate seed (Categorical opts) _v = pickG seed opts

-- a list of options presumed to be ordinal, unique and sorted, so mutate only one step at a time
data Ordinal a where
    Ordinal :: (Eq a, Hashable a) => [a] -> Ordinal a
instance Variable (Ordinal a) a where
    genRand seed (Ordinal opts) = pickG seed opts
    mutate seed (Ordinal opts) v = opts !! i'' where
        g = mkStdGen seed
        index = indexList opts
        (bl, g') = random g
        i = index ! v
        i' = (if bl then succ else pred) i
        i'' = clamp' 0 (length opts) i'

-- a variable that may take on any value within a bounded discrete range
data Discrete a where
    Discrete :: (Ord a, Random a, Num a, Real a, Integral a) => (a, a) -> a -> Discrete a
instance (Num a) => Variable (Discrete a) a where
    genRand seed (Discrete rng _base) = fst . randomR rng $ mkStdGen seed
    -- discrete/continuous are more granular; my simple approach here fixes how strongly to mutate this variable, but a better approach involges simultaneously evolving this, see e.g. http://hackage.haskell.org/package/cmaes
    -- under expensive evaluation this seems better for bayesian optimization than for evolutionary algorithms
    -- note that an additional limitation in my approach for discrete/continuous mutation is they cannot currently flip sign
    mutate  seed (Discrete (lo, hi) base) v = v' where
        g = mkStdGen seed
        (pw :: Float, g') = randomR (-1.0, 1.0) g
        v' = clamp' lo hi $ round $ realToFrac v * realToFrac base ** pw

-- `base` param indicates the maximum amount to multiply the variable by in a mutation
data Continuous a where
    Continuous :: (Ord a, Random a, Fractional a, Floating a) => (a, a) -> a -> Continuous a
instance (Fractional a) => Variable (Continuous a) a where
    genRand seed (Continuous rng _base) = fst . randomR rng $ mkStdGen seed
    mutate  seed (Continuous (lo, hi) base) v = v' where
        g = mkStdGen seed
        (pw, g') = randomR (-1.0 :: a, 1.0 :: a) g
        v' = clamp' lo hi $ v * base ** pw

-- now apply this variable logic to our HparComb parameters
-- nope, I ain't using discrete/continuous, much too expensive!

dropoutRateVar    :: Ordinal Double = Ordinal dropoutRateOpts
regularizationVar :: Ordinal Float  = Ordinal regularizationOpts
-- | skip `m=1`: must be an even number for H.
mVar              :: Ordinal Int    = Ordinal mOpts
hVar              :: Ordinal Int    = Ordinal hOpts

instance Entity HparComb Float (HparComb -> IO (EvalResult, IO ())) () IO where

  genRandom () seed = return $ HparComb
        (genRand seed dropoutRateVar)
        (genRand seed regularizationVar)
        (genRand seed mVar)
        (genRand seed hVar)

  crossover () _p seed e1 e2 = return $ Just e
    where
      anE = \bl -> if bl then e1 else e2
      g = mkStdGen seed
      [b1,b2,b3,b4] =
          take 4 $ randoms g
      e = HparComb
        (dropoutRate    $ anE b1)
        (regularization $ anE b2)
        (m              $ anE b3)
        (h              $ anE b4)

  mutation () _p seed e = return $ Just e' where
      HparComb{..} = e
      mutations :: [HparComb -> HparComb] =
          [ \e -> e { dropoutRate    = mutate seed dropoutRateVar dropoutRate }
          , \e -> e { regularization = mutate seed regularizationVar regularization }
          , \e -> e { m              = mutate seed mVar m }
          , \e -> e { h              = mutate seed hVar h }
          ]
      e' = (pickG seed mutations) e

  -- lower is better
  -- assumes a pre-existing pool; reevaluate to support continuous variables
  score getIO = Just . lossValid . fst <.> getIO

-- | main function
main :: IO ()
main = if False -- hasCuda
        then evolutionary @Gpu
        else evolutionary @Cpu

evolutionary :: forall device . (KnownDevice device, RandDTypeIsValid device 'D.Float, MatMulDTypeIsValid device 'D.Float, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float, RandDTypeIsValid device 'D.Int64) => IO ()
evolutionary = do
    EvolutionaryConfig{..} <- parseEvolutionaryConfig
    let cfg = OptimizationConfig{..}
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    putStrLn . show $ generationCfg
    let len = length hparCombs
    let g = mkStdGen seed
    let hparCombs' :: [(HparComb, IO (EvalResult, IO ()))] = join . fmap join $ (!! length exprBlocks) $
            -- featMult
            if useTypes then
                getRules @device @2 @0 cfg taskFnDataset hparCombs
            else
                getRules @device @1 @0 cfg taskFnDataset hparCombs
    let hparMap :: HashMap HparComb (IO (EvalResult, IO ())) = fromList hparCombs'
    hparMap'    :: HashMap HparComb (IO (EvalResult, IO ())) <- once `mapM` hparMap
    let getIO :: HparComb -> IO (EvalResult, IO ()) = (hparMap' !)
    let gaCfg = GAConfig 
            (ceiling . sqrt        . fromIntegral $ len) -- population size
            (ceiling . sqrt . sqrt . fromIntegral $ len) -- archive size (best entities to keep track of)
            (ceiling . sqrt . sqrt . fromIntegral $ len) -- maximum number of generations
            0.8 -- crossover rate (% of entities by crossover)
            0.2 -- mutation rate (% of entities by mutation)
            0.0 -- parameter for crossover (not used here)
            0.0 -- parameter for mutation (not used here)
            True -- whether or not to use checkpointing
            False -- don't rescore archive in each generation
    es <- evolveVerbose g gaCfg () getIO
    bests :: [(HparComb, (EvalResult, IO ()))] <- mapM (traverseToSnd getIO . snd) es
    putStrLn $ "best entities (GA): " <> show (second fst <$> bests)

    -- write results to csv
    let resultPath = printf "%s/evolutionary-%s.csv" resultFolder $ ppCfg cfg
    writeCsv resultPath gridSearchHeader $ second fst <$> bests
    putStrLn $ "data written to " <> resultPath

    -- finally evaluate the best-performing configuration on our test set
    snd . snd $ head bests
