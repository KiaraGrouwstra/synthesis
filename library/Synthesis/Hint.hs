{-# LANGUAGE ImplicitParams #-}

-- | utility functions related to the Haskell Interpreter `hint`
module Synthesis.Hint (module Synthesis.Hint) where

import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Bifunctor (second)
import Data.List (intercalate)
import GHC.Stack
import Language.Haskell.Exts.Syntax
import Language.Haskell.Interpreter
import Synthesis.Ast
import Synthesis.Types
import Synthesis.Data
import Synthesis.Utility
import System.Log.Logger

-- | imports to be loaded in the interpreter. may further specify which parts to import.
imports :: [ModuleImport]
imports =
  [ ModuleImport "Prelude"             NotQualified                   NoImportList
  , ModuleImport "Control.Exception"   NotQualified                   $ ImportList ["SomeException", "try", "evaluate"]
  , ModuleImport "Data.Bifunctor"      NotQualified                   $ ImportList ["first", "second"]
  -- , ModuleImport "Data.Set"            NotQualified                   $ ImportList ["Set"]
  -- , ModuleImport "Data.Set"            (QualifiedAs $ Just "Set")     $ ImportList ["insert"]
  , ModuleImport "Data.HashMap.Lazy"   NotQualified                   $ ImportList ["HashMap", "insert", "fromList"]
  -- , ModuleImport "Data.HashMap.Lazy"   (QualifiedAs $ Just "HashMap") $ ImportList ["fromList"]
  , ModuleImport "Control.Applicative" NotQualified                   $ ImportList ["empty"]
  , ModuleImport "Data.Hashable"       NotQualified                   $ ImportList ["Hashable"]
  ]

-- | test an interpreter monad, printing errors, returning values
interpretUnsafe :: Interpreter a -> IO a
interpretUnsafe fn = join $
  interpretSafe fn <&> \case
    Left err_ -> error $ "interpretUnsafe failed: " <> errorString err_
    Right x -> return x

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
say, debug, info, notice, warning, err, critical, alert, emergency :: String -> Interpreter ()
say = warning
-- | log: debug
debug     = liftIO . debugM     logger
-- | log: info
info      = liftIO . infoM      logger
-- | log: notice
notice    = liftIO . noticeM    logger
-- | log: warning
warning   = liftIO . warningM   logger
-- | log: error
err       = liftIO . errorM     logger
-- | log: critical
critical  = liftIO . criticalM  logger
-- | log: alert
alert     = liftIO . alertM     logger
-- | log: emergency
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
-- TODO: can I generalize Right from String to `a` and not have GHC complain?
interpretIO :: Bool -> String -> Interpreter (Either String String)
interpretIO crash_on_error cmd =
  if crash_on_error then do
    io <- interpret cmd (as :: IO String)
    Right <$> lift io
  else do
    res <- typeChecksWithDetails cmd
    either <- sequence . bimap
                  (show . fmap showError)
                  ( \_s -> do
                    io <- interpret cmd (as :: IO String)
                    lift io
                  )
                $ res
    case either of
        Left str -> do
            debug str
        _ -> pure ()
    return either

-- | get input-output pairs for a function given the inputs (for one monomorphic input type instantiation).
-- | function application is run through try-evaluate so as to Either-wrap potential run-time errors for partial functions.
-- | the reason this function needs to be run through the interpreter is I only have the function/inputs as AST,
-- | meaning I also only know the types at run-time (which is when my programs are constructed).
fnIoPairs :: Bool -> Int -> Expr -> Tp -> Expr -> Interpreter [(Expr, Either String Expr)]
fnIoPairs crash_on_error n fn_ast in_tp ins = do
  let unCurry = genUncurry n
  -- let cmd = "do; ios :: [(" ++ pp in_tp ++ ", Either SomeException _)] <- zip (" ++ ins ++ ") <$> (sequence $ try . evaluate . UNCURRY (" ++ fn_str ++ ") <$> (" ++ ins ++ " :: [" ++ pp in_tp ++ "])); return . show $ second (first show) <$> ios"
  let cmd =
        pp $
          Do l
            [ Generator l
                ( PatTypeSig l
                    (pvar "ios")
                    $ tyList
                    $ TyTuple l
                      Boxed
                      [ in_tp,
                        tyApp (tyApp (tyCon "Either") (tyCon "SomeException")) wildcard
                      ]
                )
                ( infixApp
                    (app (var "zip") ins)
                    (symbol "<$>")
                    (paren $ infixApp (var "sequence") dollar $ infixApp (infixApp (var "try") dot (infixApp (var "evaluate") dot $ app (paren unCurry) $ paren fn_ast)) (symbol "<$>") $ expTypeSig ins $ tyList in_tp)
                ),
              Qualifier l $ infixApp (infixApp (var "return") dot $ var "show") dollar $ infixApp (app (var "second") $ app (var "first") $ var "show") (symbol "<$>") $ var "ios"
            ]
  -- say cmd
  second unEitherString . unTuple2 <.> unList . parseExpr . fromRight "[]" <$> interpretIO crash_on_error cmd

-- | get the type of an expression
exprType :: Expr -> Interpreter Tp
exprType = parseType <.> typeOf . pp
