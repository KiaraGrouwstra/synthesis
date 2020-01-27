-- | utility functions related to the Haskell Interpreter `hint`
module Hint (runInterpreterMain, runInterpreter_, say, errorString, showError, interpretIO, fnIoPairs, genInputs, exprType) where

-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, ModuleImport(..), InterpreterError(..), GhcError(..), Extension(..), OptionVal(..), ModuleQualification(..), ImportList(..), setImports, setImportsQ, setImportsF, runInterpreter, interpret, typeChecks, typeChecksWithDetails, as, lift, liftIO, typeOf, languageExtensions, set)
import Language.Haskell.Exts.Syntax ( Pat(..), Type(..), Stmt(..), Boxed(..), Exp(..) )
import Types -- (Expr, Tp, parseExpr, parseType)
import Data.List (intercalate)
import Data.Either (isLeft, isRight, fromRight)
import Utility (pp)
import Ast (genUncurry)
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

-- | get input-output pairs for a function given the inputs (for one concrete input type instantiation).
-- | function application is run trough try-evaluate so as to Either-wrap potential run-time errors for partial functions.
-- | the reason this function needs to be run through the interpreter is I only have the function/inputs as AST/strings.
fnIoPairs :: Int -> String -> String -> Interpreter String
fnIoPairs n fn_str ins = do
    -- let cmd = "do; ios :: [(_, Either SomeException _)] <- zip (" ++ ins ++ ") <$> (sequence $ try . evaluate . UNCURRY (" ++ fn_str ++ ") <$> (" ++ ins ++ ")); return $ show ios"
    let insExpr = parseExpr ins
    let fn = parseExpr fn_str
    let unCurry = genUncurry n
    let cmd = pp $ Do l [ Generator l
                (PatTypeSig l
                    (pvar "ios")
                    $ TyList l $ TyTuple l Boxed [
                        wildcard,
                        tyApp (tyApp (tyCon "Either") (tyCon "SomeException")) wildcard]
                )
                (infixApp
                    (app (var "zip") insExpr)
                    (symbol "<$>")
                    (paren $ infixApp (var "sequence") dollar $ infixApp (infixApp (var "try") dot (infixApp (var "evaluate") dot $ app (paren unCurry) $ paren fn)) (symbol "<$>") insExpr)
                )
            , Qualifier l $ infixApp (var "return") dollar $ app (var "show") $ var "ios"
            ]
    -- say cmd
    checksWithDetails <- typeChecksWithDetails $ show cmd
    case checksWithDetails of
        Right s -> interpretIO cmd
        Left errors -> return $ show $ showError <$> errors

-- TODO: evaluate function calls from AST i/o from interpreter, or move to module and import to typecheck
-- TODO: refactor input to Expr? [Expr]?
-- | generate examples given a concrete type
-- | the reason this function needs to be run through the interpreter is Gen wants to know its type at compile-time.
genInputs :: Int -> String -> Interpreter [Expr]
genInputs n in_type_str = unList . parseExpr <$> interpretIO cmd
    where
        cmd = "let seed = 0 in show <$> nub <$> sample' (resize " ++ show n ++ " $ variant seed arbitrary :: Gen (" ++ in_type_str ++ "))"
        unList (List _l exps) = exps

-- | get the type of an expression
exprType :: Expr -> Interpreter Tp
exprType = fmap parseType . typeOf . pp
