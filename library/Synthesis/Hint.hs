
-- | utility functions related to the Haskell Interpreter `hint`
module Synthesis.Hint
  ( runInterpreterMain,
    interpretUnsafe,
    interpretSafe,
    say,
    debug,
    info,
    notice,
    warning,
    err,
    critical,
    alert,
    emergency,
    errorString,
    showError,
    interpretIO,
    fnIoPairs,
    exprType,
  )
where

-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Bifunctor (second)
import Data.List (intercalate)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Interpreter
import Synthesis.Ast
import Synthesis.Types
import Synthesis.Data hiding (crashOnError)
import Synthesis.Utility
import System.Log.Logger
-- import Control.Exception (SomeException, try, evaluate)

-- | imports to be loaded in the interpreter. may further specify which parts to import.
imports :: [ModuleImport]
imports =
  [ ModuleImport "Prelude" NotQualified NoImportList,
    ModuleImport "Control.Exception" NotQualified $ ImportList ["SomeException", "try", "evaluate"],
    ModuleImport "Synthesis.Types" NotQualified NoImportList
  ]

-- | run an interpreter monad, printing errors, ignoring values
-- | deprecated, replace with `interpretUnsafe`
runInterpreterMain :: Interpreter () -> IO ()
runInterpreterMain fn = join $
  interpretSafe fn <&> \case
    Left err_ -> putStrLn $ errorString err_
    Right _x -> return ()

-- | test an interpreter monad, printing errors, returning values
interpretUnsafe :: Interpreter a -> IO a
interpretUnsafe fn = join $
  interpretSafe fn <&> \case
    Left err_ -> error $ errorString err_
    Right x -> return x

-- TODO: check needed imports case-by-case for potential performance gains

-- | run an interpreter monad with imports
interpretSafe :: Interpreter a -> IO (Either InterpreterError a)
interpretSafe fn = runInterpreter $ do
  set [languageExtensions := [PartialTypeSignatures, ScopedTypeVariables, RankNTypes]]
  setImportsF imports
    >>= const fn

-- | arbitrary logger name
logger :: String
logger = "my_logger"

-- | print in the Interpreter monad
say :: String -> Interpreter ()
say = warning

-- | log: debug
-- | deprecated, not in use
debug :: String -> Interpreter ()
debug = liftIO . debugM logger

-- | log: info
-- | deprecated, not in use
info :: String -> Interpreter ()
info = liftIO . infoM logger

-- | log: notice
-- | deprecated, not in use
notice :: String -> Interpreter ()
notice = liftIO . noticeM logger

-- | log: warning
warning :: String -> Interpreter ()
warning = liftIO . warningM logger

-- | log: error
err :: String -> Interpreter ()

-- | deprecated, not in use
err = liftIO . errorM logger

-- | log: critical
-- | deprecated, not in use
critical :: String -> Interpreter ()
critical = liftIO . criticalM logger

-- | log: alert
-- | deprecated, not in use
alert :: String -> Interpreter ()
alert = liftIO . alertM logger

-- | log: emergency
-- | deprecated, not in use
emergency :: String -> Interpreter ()
emergency = liftIO . emergencyM logger

-- | run-time Language.Haskell.Interpreter compilation error
errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map showError es)
  where
    header = "ERROR: Won't compile:"
errorString e = show e

-- | show a GHC error
showError :: GhcError -> String
showError (GhcError e) = e

-- | interpret a stringified IO command, either performing an additional typecheck (slower), or just crashing on error for a bogus Either
interpretIO :: Bool -> String -> Interpreter (Either String String)
interpretIO crashOnError cmd =
  if crashOnError then do
    io <- interpret cmd (as :: IO String)
    Right <$> lift io
  else do
    res <- typeChecksWithDetails cmd
    sequence
      . bimap
        (show . fmap showError)
        ( \_s -> do
            io <- interpret cmd (as :: IO String)
            lift io
        )
      $ res

-- say e

-- | get input-output pairs for a function given the inputs (for one concrete input type instantiation).
-- | function application is run trough try-evaluate so as to Either-wrap potential run-time errors for partial functions.
-- | the reason this function needs to be run through the interpreter is I only have the function/inputs as AST/strings,
-- | meaning I also only know the types at run-time (which is when my programs are constructed).
fnIoPairs :: Bool -> Int -> Expr -> Expr -> Interpreter [(Expr, Either String Expr)]
fnIoPairs crashOnError n fn_ast ins = do
  let unCurry = genUncurry n
  -- let cmd = "do; ios :: [(_, Either SomeException _)] <- zip (" ++ ins ++ ") <$> (sequence $ try . evaluate . UNCURRY (" ++ fn_str ++ ") <$> (" ++ ins ++ ")); return $ show ios"
  let cmd =
        pp $
          Do l
            [ Generator l
                ( PatTypeSig l
                    (pvar "ios")
                    $ tyList
                    $ TyTuple l
                      Boxed
                      [ wildcard,
                        tyApp (tyApp (tyCon "Either") (tyCon "SomeException")) wildcard
                      ]
                )
                ( infixApp
                    (app (var "zip") ins)
                    (symbol "<$>")
                    (paren $ infixApp (var "sequence") dollar $ infixApp (infixApp (var "try") dot (infixApp (var "evaluate") dot $ app (paren unCurry) $ paren fn_ast)) (symbol "<$>") ins)
                ),
              Qualifier l $ infixApp (var "return") dollar $ app (var "show") $ var "ios"
            ]
  -- say cmd
  fmap (second unEitherError . unTuple2) . unList . parseExpr . fromRight "[]" <$> interpretIO crashOnError cmd

-- TODO: evaluate function calls from AST i/o from interpreter, or move to module and import to typecheck
-- TODO: refactor input to Expr? [Expr]?

-- | get the type of an expression
exprType :: Expr -> Interpreter Tp
exprType = fmap parseType . typeOf . pp
