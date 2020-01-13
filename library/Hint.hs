-- | utility functions related to the Haskell Interpreter `hint`
module Hint (say, errorString, interpretIO, handleInTp, genInputs) where

-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), interpret, as, lift, liftIO)
import Data.List (intercalate)

-- | print in the Interpreter monad
say :: String -> Interpreter ()
say = liftIO . putStrLn

-- | run-time Language.Haskell.Interpreter compilation error
errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

-- | interpret a stringified IO command
interpretIO :: String -> Interpreter String
interpretIO cmd = do
    io <- interpret cmd (as :: IO String)
    lift io

-- | get input-output pairs for a function given the inputs (for one concrete input type instantiation)
handleInTp :: String -> String -> Interpreter String
handleInTp fn_str ins = interpret ("show $ zip (" ++ ins ++ ") $ (" ++ fn_str ++ ") <$> (" ++ ins ++ ")") (as :: String)

-- TODO: evaluate function calls from AST i/o from interpreter, or move to module and import to typecheck
-- | generate examples given a concrete type (to and from string to keep a common type, as we need this for the run-time interpreter anyway)
genInputs :: String -> Interpreter String
genInputs in_type_str = interpretIO $ "let n = 10; seed = 0 in show <$> nub <$> sample' (resize n $ variant seed arbitrary :: Gen (" ++ in_type_str ++ "))"
