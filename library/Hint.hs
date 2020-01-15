-- | utility functions related to the Haskell Interpreter `hint`
module Hint (runInterpreter_, say, errorString, interpretIO, fnIoPairs, genInputs) where

-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), runInterpreter, interpret, as, lift, liftIO)
import Data.List (intercalate)

-- | run an interpreter monad, printing any errors
runInterpreter_ :: Interpreter () -> IO ()
runInterpreter_ fn = do
    r <- runInterpreter fn
    case r of
        Left err -> putStrLn $ errorString err
        Right () -> return ()

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
fnIoPairs :: String -> String -> Interpreter String
fnIoPairs fn_str ins = interpret ("show $ zip (" ++ ins ++ ") $ (" ++ fn_str ++ ") <$> (" ++ ins ++ ")") (as :: String)

-- TODO: evaluate function calls from AST i/o from interpreter, or move to module and import to typecheck
-- | generate examples given a concrete type (to and from string to keep a common type, as we need this for the run-time interpreter anyway)
genInputs :: Int -> String -> Interpreter String
genInputs n in_type_str = interpretIO $ "let seed = 0 in show <$> nub <$> sample' (resize " ++ show n ++ " $ variant seed arbitrary :: Gen (" ++ in_type_str ++ "))"
