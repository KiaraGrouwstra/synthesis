module Utility (Tp, Expr, Item(..), say, errorString, interpretIO, fnStr, undef, returnType, randomType, fnInTp, typeNode, polyTypeNode, skeleton, flatten, pick) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Syntax ( Exp(ExpTypeSig), Type(TyFun, TyCon, TyApp), Name(Ident), QName(UnQual) ) -- , SpecialCon(ExprHole), TyVar
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.SrcLoc ( SrcSpan(..), SrcSpanInfo(..), srcInfoSpan, srcInfoPoints )
-- import Test.QuickCheck ( Gen, arbitrary, sample, sample', variant, generate, resize )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), interpret, as, typeOf, lift, liftIO) -- , MonadInterpreter, infer, eval, kindOf, typeChecks, runInterpreter, setImports
import Data.List (intercalate) -- , replicate, nub
import System.Random (randomRIO)
-- import Control.Monad (forM_)
import Data.Hashable (Hashable)
import qualified Data.Text as T
-- import Data.Text (Text(..), unpack)
import qualified Data.Text.Array as TA

-- monad stuff

say :: String -> Interpreter ()
say = liftIO . putStrLn

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

interpretIO :: String -> Interpreter String
interpretIO cmd = do
    io <- interpret cmd (as :: IO String)
    lift io

-- type stuff

-- these verbose types annoy me so let's alias them
type Tp = Type SrcSpanInfo
type Expr = Exp SrcSpanInfo

fnStr :: String -> String -> String
fnStr i o  = i ++ " -> " ++ o

undef :: String -> String
undef tp = "(undefined :: " ++ tp ++ ")"

-- str = show $ funResultTy (typeOf (reverse :: [Char] -> [Char])) $ typeOf "abc"
returnType :: String -> String -> Interpreter String
returnType fn_tp_str par_tp_str = typeOf $ undef fn_tp_str ++ undef par_tp_str

randomType :: Int -> IO Tp
randomType nestLimit = do
    io <- pick $ case nestLimit of
            0 -> simples
            _ -> simples ++ monos
    io
    where
        simples = [ simple "Bool"
                  , simple "Int"
                  ]
        monos = [ mono "[]"
                ]
        simple = return . typeNode
        mono str = do
            tp <- randomType (nestLimit - 1)
            return $ polyTypeNode str tp

fnInTp :: String -> Interpreter Tp
fnInTp fn_tp_str = do
    let tp_ast = fromParseResult (parse ("_ :: " ++ fn_tp_str) :: ParseResult Expr)
    tp_fn <- case tp_ast of
                ExpTypeSig _mdl _exp tp -> return tp
                x -> fail $ "expected ExpTypeSig, not" ++ show x
    in_tp <- case tp_fn of
                TyFun _mdl i _o -> return i
                x -> fail $ "expected TyFun, not " ++ show x
    -- say $ prettyPrint in_tp
    return in_tp

-- ast stuff

l :: SrcSpanInfo
l = SrcSpanInfo {srcInfoSpan = spn, srcInfoPoints = []}
    where
        spn = SrcSpan "<unknown>.hs" 1 1 1 1

typeNode :: String -> Tp
typeNode str = TyCon l $ UnQual l $ Ident l str

polyTypeNode :: String -> Tp -> Tp
polyTypeNode str tp = TyApp l (typeNode str) tp

-- -- can't get TypeRep for polymorphic types
-- skeleton :: TypeRep -> Expr
-- skeleton rep = expr
--     where
--         io = typeRepArgs rep
--         hole = Var l $ Special l $ ExprHole l
--         i = typeNode . show $ head io
--         o = typeNode . show $ last io
--         tp_fn = TyFun l i o
--         expr = ExpTypeSig l hole tp_fn

skeleton :: String -> Expr
skeleton fn_tp_str = expr
    where
        src = "_ :: " ++ fn_tp_str
        expr = fromParseResult (parse src :: ParseResult Expr)

-- misc

data Item a = One [a] | Many [Item a]

flatten :: Item a -> [a]
flatten (One x) = x
flatten (Many x) = concatMap flatten x

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

-- -- ‘hashWithSalt’ is not a (visible) method of class ‘Hashable’
-- -- https://github.com/haskell-infra/hackage-trustees/issues/139
-- instance (Type l) => Hashable (Type l) where
--     -- hash = id
--     hashWithSalt = \n -> hash
--     -- hashWithSalt salt tp = case (unpack $ prettyPrint tp) of
--     --         -- https://github.com/tibbe/hashable/blob/cc4ede9bf7821f952eb700a131cf1852d3fd3bcd/Data/Hashable/Class.hs#L641
--     --         T.Text arr off len -> hashByteArrayWithSalt (TA.aBA arr) (off `shiftL` 1) (len `shiftL` 1) salt
--     -- --  :: Int -> a -> Int
--     -- -- hashWithSalt salt tp = prettyPrint tp
--     -- -- hashWithSalt = defaultHashWithSalt
