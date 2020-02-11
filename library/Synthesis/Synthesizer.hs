-- | synthesizer logic
module Synthesis.Synthesizer
  ( main,
  )
where

import Control.Monad (forM_)
import Data.HashMap.Lazy
  ( HashMap,
    (!),
  )
import Data.Store (decodeIO)
import qualified Data.ByteString as BS
import Language.Haskell.Interpreter (Interpreter, liftIO)
import Synthesis.Configs
  ( filePath,
  )
import Synthesis.Types
  ( expTypeSig,
  )
import Synthesis.Ast
  ( letRes,
  )
import Synthesis.Hint
  ( runInterpreterMain,
    say,
  )
import Synthesis.Orphanage ()
import Synthesis.Data (Expr, Tp, Stuff (..))
import Synthesis.Utility
  ( pp_,
    pickKeysSafe,
  )

-- | main function, run program in our interpreter monad
main :: IO ()
main = runInterpreterMain program

-- | run our program in the interpreter
program :: Interpreter ()
program = do
  bs :: BS.ByteString <- liftIO $ BS.readFile filePath
  Stuff { fn_types = fn_types
        , fn_in_type_instance_outputs = fn_in_type_instance_outputs
        , fn_in_type_instantiations = fn_in_type_instantiations
        , rest_instantiation_inputs = rest_instantiation_inputs
        , datasets = datasets
        } :: Stuff <- liftIO $ decodeIO bs
  let (train, validation, test) :: ([Expr], [Expr], [Expr]) = datasets
  say $      "train: " ++ show (length train)
  say $ "validation: " ++ show (length validation)
  say $       "test: " ++ show (length test)

  -- TODO: learner
  say "\n\nenumerating function i/o examples:"
  -- say "\n\nfinding fits!"
  forM_ train $ \ast -> do
    let fn_type :: Tp = fn_types ! ast
    say "================================================"
    say $ "\n" ++ pp_ (expTypeSig (letRes ast) fn_type)
    let in_type_instance_outputs :: HashMap [Tp] String = fn_in_type_instance_outputs ! ast
    -- say "\nin_type_instance_outputs:"
    say $ pp_ in_type_instance_outputs
    -- let instantiations :: [[Tp]] = fn_in_type_instantiations ! ast
    -- say "\ninstantiations:"
    -- say $ pp_ instantiations
    -- let inst_io_pairs :: HashMap [Tp] String = pickKeysSafe instantiations in_type_instance_outputs
    -- say "\ninst_io_pairs:"
    -- say $ pp_ inst_io_pairs
-- -- synthesize matching programs by brute force
-- let inst_inputs :: HashMap String String = pickKeys instantiations rest_instantiation_inputs
-- candidate_ios :: [HashMap String String] <- forM programs $ forM inst_inputs . fnIoPairs . pp
-- let candidates :: [Expr] = fmap (letRes . fst) $ filter (\(_expr, inst_ios) -> PP inst_io_pairs == PP inst_ios) $ zip programs candidate_ios
-- say $ pp_ candidates
