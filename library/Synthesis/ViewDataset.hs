{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

-- | view some info about a dataset
module Synthesis.ViewDataset (module Synthesis.ViewDataset) where

import Data.HashMap.Lazy
import Data.Store (decodeIO)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
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
    bs :: BS.ByteString <- BS.readFile filePath
    taskFnDataset :: TaskFnDataset <- decodeIO bs
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
        let in_type_instance_outputs :: HashMap [Tp] [(Expr, Either String Expr)] = fnInTypeInstanceOutputs ! ast
        putStrLn $ pp_ in_type_instance_outputs
