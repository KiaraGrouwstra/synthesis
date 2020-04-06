{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

-- | synthesizer logic
module Synthesis.Synthesizer
    ( module Synthesis.Synthesizer
    --   main,
    )
where

-- import Control.Monad (forM_, forM_)
-- import Data.HashMap.Lazy
--     ( HashMap,
--       (!),
--     )
import Data.Store (decodeIO)
import qualified Data.ByteString as BS
import Language.Haskell.Interpreter (Interpreter, liftIO)
-- import Synthesis.Types
-- import Synthesis.Ast
import Synthesis.Hint
import Synthesis.Orphanage ()
import Synthesis.Data (TaskFnDataset (..), SynthesizerConfig (..))  -- Expr, Tp, 
import Synthesis.Configs
-- import Synthesis.Utility
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.NSPS

-- | main function, run program in our interpreter monad
main :: IO ()
main = runInterpreterMain program

-- dummy values to trick the compiler
type T = 42
type N_train = 111
type N_validation = 222
type N_test = 333

-- | run our program in the interpreter
program :: Interpreter ()
program = do
    synthesizer_config :: SynthesizerConfig <- liftIO parseSynthesizerConfig
    let SynthesizerConfig{..} = synthesizer_config
    bs :: BS.ByteString <- liftIO $ BS.readFile filePath
    taskFnDataset :: TaskFnDataset <- liftIO $ decodeIO bs

    -- liftIO $ printTaskFns taskFnDataset train_set
    liftIO $ train @M @BatchSize @Symbols @Rules @T @N_train @N_validation @N_test synthesizer_config taskFnDataset

-- -- | print info on task functions
-- -- | deprecated, not in use
-- printTaskFns :: TaskFnDataset -> [Expr] -> Interpreter ()
-- printTaskFns TaskFnDataset{..} train_set = do
--     say "\n\nenumerating function i/o examples:"
--     -- say "\n\nfinding fits!"
--     forM_ train_set $ \ast -> do
--         let fn_type :: Tp = fnTypes ! ast
--         say "================================================"
--         say $ "\n" ++ pp_ (expTypeSig (letRes ast) fn_type)
--         let in_type_instance_outputs :: HashMap [Tp] [(Expr, Either String Expr)] = fnInTypeInstanceOutputs ! ast
--         -- say "\nin_type_instance_outputs:"
--         say $ pp_ in_type_instance_outputs

--         -- let instantiations :: [[Tp]] = fnInTypeInstantiations ! ast
--         -- say "\ninstantiations:"
--         -- say $ pp_ instantiations
--         -- let inst_io_pairs :: HashMap [Tp] String = pickKeysSafe instantiations in_type_instance_outputs
--         -- say "\ninst_io_pairs:"
--         -- say $ pp_ inst_io_pairs
--         -- bruteForce programs inst_io_pairs instantiations restInstantiationInputs

-- -- | synthesize matching programs by brute force
-- -- | deprecated, not in use
-- bruteForce :: Interpreter ()
-- bruteForce programs inst_io_pairs instantiations restInstantiationInputs = do
--     let inst_inputs :: HashMap String String = pickKeys instantiations restInstantiationInputs
--     candidate_ios :: [HashMap String String] <- forM programs $ forM inst_inputs . fnIoPairs crashOnError . pp
--     let candidates :: [Expr] = fmap (letRes . fst) $ filter (\(_expr, inst_ios) -> pp_ inst_io_pairs == pp_ inst_ios) $ zip programs candidate_ios
--     say $ pp_ candidates
