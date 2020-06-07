{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

-- | view some info about a dataset
module Synthesis.ViewDataset (module Synthesis.ViewDataset) where

import Data.HashMap.Lazy
import Control.Monad (forM_)
import Data.Yaml
import Synthesis.Hint
import Synthesis.Orphanage ()
import Synthesis.Data
import Synthesis.Configs
import Synthesis.Utility
import Synthesis.Types
import Synthesis.Ast

-- | main function
main :: IO ()
main = do
    cfg :: ViewDatasetConfig <- parseViewDatasetConfig
    putStrLn $ show cfg
    let ViewDatasetConfig{..} = cfg
    taskFnDataset :: TaskFnDataset <- decodeFileThrow taskPath
    let TaskFnDataset{..} = taskFnDataset
    let (train_set, validation_set, test_set) = datasets
    putStrLn $ show generationCfg
    -- putStrLn $ show taskFnDataset
    printTaskFns taskFnDataset train_set

-- | print info on task functions
printTaskFns :: TaskFnDataset -> [Expr] -> IO ()
printTaskFns TaskFnDataset{..} train_set = do
    putStrLn "\n\nenumerating function i/o examples:"
    forM_ train_set $ \ast -> do
        let fn_type :: Tp = fnTypes ! ast
        putStrLn "================================================"
        putStrLn $ "\n" ++ pp_ (expTypeSig (letRes ast) fn_type)
        let tp_ios :: HashMap (Tp, Tp) [(Expr, Either String Expr)] = fnTypeIOs ! ast
        putStrLn $ pp_ tp_ios
