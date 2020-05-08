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

-- | Bayesian Optimization using Gaussian Processes (GPs), loosely based on Sherpa / GPyOpt
module Synthesis.Synthesizer.BayesOpt (module Synthesis.Synthesizer.BayesOpt) where

import GHC.Float (clamp)
import GHC.Real (infinity)
import Control.Monad (replicateM)
import Systme.Random

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

import           Synthesis.Synthesizer.GP

fn = pure
objective = evaluate SingleObjective fn

space = HM.fromList
    [ ("var_1", Continuous (-5.0, 10.0))
    , ("var_2", Discrete [1,2,3])
    , ("var_3", Categorical [0,1])  -- ["a","b"]
    ]

-- http://hackage.haskell.org/package/HasGP-0.1/docs/src/HasGP-Demos-ClassificationDemo2.html#stopEP
-- import HasGP

x <- getSamples space RandomDesign 5
acquisition = expectedImprovement { space=space }

run_optimization model space objective acquisition x 10

run_optimization model=GPModel space objective acquisition x max_iter = do
    (y, costs) = objective x
    cost_update = GPModel
    -- cost_update x costs
    (x,y,costs) <- foldLoop (x,y,costs) max_iter $ \ state@(x,y,costs) epoch -> do
        cost_update x costs
        updateModel model x $ batchNorm y
        -- _save_model_parameter_values()
        f = - (computeAcq acquisition x) / fst (cost_withGradients acquisition x)
        f_df = acquisitionFunctionWithGradients acquisition
        x' = fst $ optimize optimizer space f f_fd
        y', costs' = objective x'
        pure (x':x,y':y,costs':costs)

    y_best = scan min y  -- cumulative best
    x_best = x[argmin y,:]
    fx_best = min y

batchNorm :: D.Tensor -> D.Tensor
batchNorm y = let
    y_norm = y `F.sub` F.mean y
    std = D.asValue $ F.std y
    in if std > 0.0 then I.div y_norm std else y_norm

-- Variable

data Variable =
      Continuous (Float, Float)
    | Discrete [Int]
    | Categorical [Int]
-- | Bandit
-- | ordinal

-- DesignSpace

type DesignSpace = HM.HashMap String Variable

-- ExperimentDesign

data ExperimentDesign = RandomDesign

getSamples :: DesignSpace -> ExperimentDesign -> Int -> IO D.Tensor
getSamples space RandomDesign n = replicateM n $ sample `mapM` space
  where sample var = case var of
          Continuous (lo, hi) -> randomRIO (lo, hi)
          Discrete ints -> fromIntegral <$> pick ints :: IO Float
          Categorical opts -> fromIntegral <$> pick opts :: IO Float

-- | randomly pick an item from a list
pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)

space = HM.fromList
    [ ("var_1", Continuous (-5.0, 10.0))
    , ("var_2", Discrete [1,2,3])
    , ("var_3", Categorical [0,1])
    ]

getSamples space RandomDesign 3

-- GPyOpt.experiment_design.initial_design('random', space, 3)
-- array([[ 4.68653732,  8.40391373],
--        [ 2.09472782, 14.62744825],
--        [ 2.30357361, 11.98082235]])

-- Objective

data Objective = SingleObjective

evaluate :: Objective -> IO a -> IO (a, Integer)
evaluate SingleObjective io = do
    start <- getCPUTime
    y <- io
    end <- getCPUTime
    let time = end - start
    return (y, time)

-- BOModel

data BOModel = GPModel

updateModel :: D.Tensor -> D.Tensor -> IO (D.Tensor, D.Tensor)
updateModel GPModel x y = do
    -- TODO: am I using input_dim right?
    (postMu, postCov) <- computePosterior (D.asValue $ select'' 0 input_dim x) (D.asValue y) tRange
    return (postMu, postCov)
    -- let (postMu, postCov) = ?
    -- let num_restarts = 5
    -- mvnSampPost <- mvnCholesky postCov (D.elems postMu) num_restarts
    -- return $ addMean postMu $ F.meanDim 1 $ mvnSampPost
    where 
      -- TODO
      scale = 0.1
      axisDim = 7
      tRange = (*) scale <$> (fromIntegral <$> [0 .. (axisDim - 1)])

getFmin :: BOModel -> Int
getFmin GPModel{..} = model.predict(model.x)[0].min()

predict :: BOModel -> D.Tensor -> (D.Tensor, D.Tensor)
predict GPModel{..} x = (m, F.sqrt (clamp 1e-10 inf v)) where
    if x.ndim==1: x = x[None,:]
    (m, v) = model.predict(x, full_cov=False, include_likelihood=True?)

predictWithGradients :: BOModel -> D.Tensor -> (D.Tensor, D.Tensor, D.Tensor, D.Tensor)
predictWithGradients GPModel{..} x = (m, F.sqrt (clamp 1e-10 inf v), dmdx, dsdx) where
    if x.ndim==1: x = x[None,:]
    (m, v) = model.predict(x)
    (dmdx, dvdx) = model.predictive_gradients(x)
    dmdx = dmdx[:,:,0]
    dsdx = dvdx / (2 * F.sqrt v)

-- Acquisition

data AcquisitionType = ExpectedImprovement { space :: DesignSpace, model :: BOModel, optimizer :: Optimizer, cost_withGradients :: D.Tensor -> D.Tensor, jitter :: Float }
expectedImprovement = ExpectedImprovement { space=HM.empty, model=GPModel, optimizer=AcquisitionOptimizer, jitter=0.01, cost_withGradients = constantCostWithGradients }

constantCostWithGradients :: D.Tensor -> D.Tensor
constantCostWithGradients x =
    (D.ones [D.dim x 0, 1] D.float_opts, D.zerosLike x)

acquisitionFunctionWithGradients :: AcquisitionType -> D.Tensor -> (D.Tensor, D.Tensor)
acquisitionFunctionWithGradients acquisition x = (-f_acq_cost, -df_acq_cost) where
    (f_acqu, df_acqu) = computeAcqWithGradients acquisition x
    (cost_x, cost_grad_x) = cost_withGradients acquisition x
    f_acq_cost = f_acqu / cost_x
    df_acq_cost = (df_acqu * cost_x - f_acqu * cost_grad_x) / (cost_x ** 2)

computeAcq :: AcquisitionType -> D.Tensor -> D.Tensor
computeAcq ExpectedImprovement{..} x = f_acqu where
    (m, s) = predict model x
    fmin = getFmin model
    (phi, Phi, u) = getQuantiles jitter fmin m s
    f_acqu = s * (u * Phi + phi)

computeAcqWithGradients :: AcquisitionType -> D.Tensor -> (D.Tensor, D.Tensor)
computeAcqWithGradients ExpectedImprovement{..} x = (f_acqu, df_acqu) where
    (m, s, dmdx, dsdx) = predictWithGradients model x
    fmin = getFmin model
    (phi, Phi, u) = getQuantiles jitter fmin m s
    f_acqu = s * (u * Phi + phi)
    df_acqu = dsdx * phi - Phi * dmdx

getQuantiles :: Float -> D.Tensor? -> D.Tensor? -> D.Tensor? -> (D.Tensor?, D.Tensor?, D.Tensor?)
getQuantiles acquisition_par fmin m s = (phi, Phi, u) where
    u = (fmin - m - acquisition_par) / clamp eps inf s
    phi = exp (-0.5 * u ** 2) / sqrt (2 * pi)
    Phi = 0.5 * I.erfc ((-u) / sqrt 2)

-- Optimizer

data OptimizerType = AcquisitionOptimizer { optimizer :: String }
-- acquisitionOptimizer = { optimizer="lbfgs" }

optimize :: OptimizerType -> DesignSpace -> (D.Tensor -> D.Tensor) -> D.Tensor -> (D.Tensor, D.Tensor)
optimize AcquisitionOptimizer{..} space f f_df = (x_min, fx_min) where
    anchor_points = get $ (ObjectiveAnchorPointsGenerator objective?) num_anchor=5 ??? space random_design_type f
    optimized_points = [apply_optimizer(optimizer a f df=None f_df space) for a in anchor_points]
    (x_min, fx_min) = minBy (lambda t:t[1]) optimized_points

-- AnchorPointsGenerator

data AnchorPointsGeneratorType = ObjectiveAnchorPointsGenerator { objective :: D.Tensor -> D.Tensor }

data AnchorPointsGenerator = AnchorPointsGenerator { space :: DesignSpace, design_type :: ExperimentDesign, num_samples :: Int, anchorPointsGeneratorType :: AnchorPointsGeneratorType }

getAnchorPointScores :: AnchorPointsGeneratorType -> D.Tensor
getAnchorPointScores ObjectiveAnchorPointsGenerator{..} = D.reshape [-1] . objective

get :: AnchorPointsGenerator -> Int -> D.Tensor
get AnchorPointsGenerator{..} num_anchor = anchor_points where
    x :: Set D.Tensor = Set.fromList . unDim 0 $ getSamples space design_type num_samples
    scores :: [Float] = D.asValue $ getAnchorPointScores anchorPointsGeneratorType x
    anchor_points = take num_anchor (minBy ? scores)

space = HM.fromList
    [ ("var_1", Continuous (-3.0, 1.0))
    , ("var_2", Discrete [0,1,2,3])
    , ("var_3", Categorical [0,1])
    ]
dummyObjective x = F.sumDim $ (Dim 1) F.removeDim D.Float $ F.matmul x x
generator = AnchorPointsGenerator design_space RandomDesign 10

F.all $ getAnchorPointScores (ObjectiveAnchorPointsGenerator dummyObjective) (D.asTensor [[0.0],[1.0],[2.0]]) == D.asTensor [0.0, 1.0, 4.0 :: Float]

tolerance = 1e-8
seed = 666
manual_seed_L $ fromIntegral seed
I.normAll (get generator 2 `F.sub` D.asTensor [[-0.02338332, 2.0, 1.0, 0.0],[ 0.09791782, 2.0, 1.0, 0.0]]) `I.ltScalar` tolerance

-- space = [
--     {'name': 'var_1', 'type': 'continuous', 'domain':(-3,1), 'dimensionality': 1},
--     {'name': 'var_2', 'type': 'discrete', 'domain': (0,1,2,3)},
--     {'name': 'var_3', 'type': 'categorical', 'domain': (0, 1)}
-- ]
-- design_space = Design_space(space)
-- np.random.seed(666)
-- design_type = "random"
-- dummyObjective = lambda X : np.sum(X*X, axis=1)
-- generator = ObjectiveAnchorPointsGenerator(design_space, design_type, dummyObjective, num_samples=10)
-- assert np.all(generator.get_anchor_point_scores(np.arange(3).reshape(3,1)) == np.array([0.0, 1.0, 4.0]))
-- assert np.linalg.norm(generator.get(num_anchor=2) - np.array([[-0.02338332, 2., 1., 0.],[ 0.09791782, 2., 1., 0.]])) < tolerance

