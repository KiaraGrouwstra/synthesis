{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, DataKinds #-}

-- | utility functions
module Utility (Tp, Expr, Item(..), say, errorString, interpretIO, randomType, typeNode, polyTypeNode, skeleton, flatten, pick, groupByVal, toMapBy, NestedTuple, flattenTuple, fnTypeIO, genTypes, instantiateTypes) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Syntax ( Exp, Type(TyFun, TyCon, TyApp, TyVar), Name(Ident), QName(UnQual) )
import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.SrcLoc ( SrcSpan(..), SrcSpanInfo(..), srcInfoSpan, srcInfoPoints )
-- TODO: pre-compile for performance, see https://github.com/haskell-hint/hint/issues/37
import Language.Haskell.Interpreter (Interpreter, InterpreterError(..), GhcError(..), interpret, as, lift, liftIO)
import Data.List (intercalate, replicate, nub)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap, fromList, (!))
import GHC.Exts (groupWith)
import Data.Bifoldable (biList)

-- monad stuff

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

-- type stuff

-- these verbose types annoy me so let's alias them
type Tp = Type SrcSpanInfo
type Expr = Exp SrcSpanInfo
-- type Fn = TyFun SrcSpanInfo (Type SrcSpanInfo a) (Type SrcSpanInfo b)

-- | randomly generate a concrete type, used to fill type variables
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

-- | extract the input and output types from a function type
fnTypeIO :: Tp -> (Tp, Tp)
fnTypeIO tp = case tp of
                TyFun _l a b -> (a, b)
                -- x -> fail $ "unexpected " ++ show x

-- TODO: take into account type variable constraints
-- | generate any combinations of a polymorphic type filled using a list of concrete types
instantiateTypes :: [Tp] -> Tp -> [Tp]
instantiateTypes tps tp = fillTypeVars tp <$> instantiateTypeVars tps (findTypeVars tp)

-- | find the type variables and their occurrences
findTypeVars :: Tp -> [String]
findTypeVars tp = nub $ findTypeVars_ tp

findTypeVars_ :: Tp -> [String]
findTypeVars_ tp = let f = findTypeVars_ in case tp of
            TyVar _l _name -> [prettyPrint tp]
            TyApp _l a b -> f a ++ f b
            TyFun _l a b -> f a ++ f b
            _ -> []

-- | instantiate type variables
instantiateTypeVars :: [Tp] -> [String] -> [HashMap String Tp]
instantiateTypeVars tps ks = fromList . zip ks <$> (sequence (replicate (length ks) tps))

-- | substitute all type variable occurrences
fillTypeVars :: Tp -> HashMap String Tp -> Tp
fillTypeVars tp substitutions = let f = flip fillTypeVars substitutions in case tp of
    TyVar _l _name -> substitutions ! prettyPrint tp
    TyApp _l a b -> TyApp _l (f a) $ f b
    TyFun _l a b -> TyFun _l (f a) $ f b
    _ -> tp

-- | generate a number of types to be used in type variable substitution
genTypes :: IO (Item Tp)
genTypes = Many . fmap (One . pure) <$> replicateM maxInstances (randomType nestLimit)
    where
        maxInstances = 5  -- may get less after nub filters out duplicate type instances
        nestLimit = 0 -- high values make for big logs while debugging...

-- ast stuff

-- | dummy source span info, because I don't care
l :: SrcSpanInfo
l = SrcSpanInfo {srcInfoSpan = spn, srcInfoPoints = []}
    where
        spn = SrcSpan "<unknown>.hs" 1 1 1 1

-- | create a monomorphic type node
typeNode :: String -> Tp
typeNode str = TyCon l $ UnQual l $ Ident l str

-- | create a polymorphic type node
polyTypeNode :: String -> Tp -> Tp
polyTypeNode str = TyApp l (typeNode str)

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

-- | given a string representation of a function type, create a skeletal
-- | AST node representing the function consisting of a hole
skeleton :: String -> Expr
skeleton fn_tp_str = expr
    where
        src = "_ :: " ++ fn_tp_str
        expr = fromParseResult (parse src :: ParseResult Expr)

-- misc

-- | a homogeneous nested list
data Item a = One [a] | Many [Item a]

-- | flatten a nested list
flatten :: Item a -> [a]
flatten (One x) = x
flatten (Many x) = concatMap flatten x

-- | a homogeneous nested tuple
data NestedTuple a = SingleTuple (a, a) | DeepTuple (a, NestedTuple a)

-- | flatten a nested tuple
flattenTuple :: NestedTuple a -> [a]
flattenTuple tpl = case tpl of
    SingleTuple tpl_ -> biList tpl_
    DeepTuple (x, xs) -> x : flattenTuple xs

-- | randomly pick an item from a list
pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- | group a list of k/v pairs by values, essentially inverting the original HashMap
groupByVal :: (Hashable v, Ord v) => [(k, v)] -> HashMap v [k]
groupByVal = fromList . fmap (\pairs -> (snd $ head pairs, fst <$> pairs)) . groupWith snd

-- | create a HashMap by mapping over a list of keys
toMapBy :: [String] -> (String -> a) -> HashMap String a
toMapBy ks fn = fromList $ zip ks $ fn <$> ks

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
