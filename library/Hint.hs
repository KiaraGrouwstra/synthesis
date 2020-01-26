-- | utility functions related to the Haskell Interpreter `hint`
module Hint (runInterpreterMain, runInterpreter_, say, errorString, showError, interpretIO, fnIoPairs, genInputs, exprType) where

-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, ModuleImport(..), InterpreterError(..), GhcError(..), Extension(..), OptionVal(..), ModuleQualification(..), ImportList(..), setImports, setImportsQ, setImportsF, runInterpreter, interpret, typeChecks, typeChecksWithDetails, as, lift, liftIO, typeOf, languageExtensions, set)
import Types (Expr, Tp, parseExpr, parseType)
import Data.List (intercalate)
import Data.Either (isLeft, isRight, fromRight)
import Utility (pp)
import System.Log.Logger (getRootLogger, logL, Priority(..), warningM)
import Control.Monad (join)

-- | modules to be loaded in the interpreter
modules :: [String]
modules = ["Prelude", "Data.List", "Test.QuickCheck", "Control.Exception"]

-- | potentially qualified imports to be loaded in the interpreter
qualified :: [(String, Maybe String)]
qualified = [("Prelude", Nothing), ("Data.Vector", Just "V")]

-- | imports to be loaded in the interpreter. may further specify which parts to import.
imports :: [ModuleImport]
imports = [ ModuleImport "Prelude" NotQualified NoImportList
          , ModuleImport "Data.List" NotQualified NoImportList
          , ModuleImport "Test.QuickCheck" NotQualified NoImportList
          , ModuleImport "Control.Exception" NotQualified $ ImportList ["SomeException", "try", "evaluate"]
          ]

-- | run an interpreter monad, printing errors, ignoring values
runInterpreterMain :: Interpreter () -> IO ()
runInterpreterMain fn = do
    r <- runInterpreter_ fn
    case r of
        Left err -> putStrLn $ errorString err
        Right _x -> return ()

-- TODO: check needed imports case-by-case for potential performance gains
-- | run an interpreter monad with imports
runInterpreter_ :: Interpreter a -> IO (Either InterpreterError a)
runInterpreter_ fn =
    runInterpreter $ do
        set [languageExtensions := [PartialTypeSignatures, ScopedTypeVariables]]
        -- setImports modules >>= const fn
        -- setImportsQ qualified >>= const fn
        setImportsF imports >>= const fn

-- | print in the Interpreter monad
say :: String -> Interpreter ()
-- say = liftIO . putStrLn
-- say = liftIO . logL getRootLogger INFO
say = liftIO . warningM "my_logger"

-- | run-time Language.Haskell.Interpreter compilation error
errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map showError es)
  where
    header = "ERROR: Won't compile:"
errorString e = show e

-- | show a GHC error
showError :: GhcError -> String
showError (GhcError e) = e

-- | interpret a stringified IO command
interpretIO :: String -> Interpreter String
interpretIO cmd = do
    io <- interpret cmd (as :: IO String)
    lift io

-- | get input-output pairs for a function given the inputs (for one concrete input type instantiation)
-- | the reason this function needs to be run through the interpreter is I only have the function/inputs as AST/strings.
fnIoPairs :: String -> String -> Interpreter String
fnIoPairs fn_str ins = do
    let cmd = "do; ios :: [(_, Either SomeException _)] <- zip (" ++ ins ++ ") <$> (sequence $ try . evaluate . (" ++ fn_str ++ ") <$> (" ++ ins ++ ")); return $ show ios"
    -- say $ "cmd: " ++ show cmd
    checksWithDetails <- typeChecksWithDetails $ show cmd
    case checksWithDetails of
        Right s -> interpretIO cmd
        Left errors -> return $ show $ showError <$> errors

-- TODO: evaluate function calls from AST i/o from interpreter, or move to module and import to typecheck
-- | generate examples given a concrete type (to and from string to keep a common type, as we need this for the run-time interpreter anyway)
-- | the reason this function needs to be run through the interpreter is Gen wants to know its type at compile-time.
genInputs :: Int -> String -> Interpreter String
genInputs n in_type_str = interpretIO $ "let seed = 0 in show <$> nub <$> sample' (resize " ++ show n ++ " $ variant seed arbitrary :: Gen (" ++ in_type_str ++ "))"

-- | get the type of an expression
exprType :: Expr -> Interpreter Tp
exprType = fmap parseType . typeOf . pp
