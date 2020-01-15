{-# LANGUAGE LambdaCase #-}

-- | utility functions specifically related to types
module Types (Tp, Expr, Hole, randomType, randomFnType, tyCon, tyApp, fnTypeIO, genTypes, instantiateTypes, holeType, varNode, tyVar, qName, l, findTypeVars, instantiateTypeVars, fillTypeVars) where

import Language.Haskell.Exts.Pretty ( prettyPrint )
import Language.Haskell.Exts.Syntax ( Exp(..), SpecialCon(..), Type(..), Name(..), QName(..), Type(..) )
-- import Language.Haskell.Exts.Parser ( ParseResult, parse, fromParseResult )
import Language.Haskell.Exts.SrcLoc ( SrcSpan(..), SrcSpanInfo(..), srcInfoSpan, srcInfoPoints )
import Data.List (replicate, nub)
import Control.Monad (join, replicateM)
import Data.HashMap.Lazy (HashMap, fromList, (!))
import Utility (Item(..), pick)

-- these verbose types annoy me so let's alias them
type Tp = Type SrcSpanInfo
type Expr = Exp SrcSpanInfo
type Hole = SpecialCon SrcSpanInfo -- ExprHole
-- type Fn = TyFun SrcSpanInfo (Type SrcSpanInfo a) (Type SrcSpanInfo b)

-- | randomly generate a type
-- TODO: allow generating new type vars
randomType :: Bool -> Bool -> Int -> [String] -> Int -> IO Tp
randomType allowAbstract allowFns nestLimit typeVars tyVarCount = join $ pick options
    where
        f = randomType allowAbstract allowFns (nestLimit - 1)
        options :: [IO Tp] = base ++ abstracts ++ fns
        base :: [IO Tp] = simples ++ tpVars ++ if nestLimit > 0 then monos else []
        abstracts :: [IO Tp] = if allowAbstract then [tpVar] else []
        fns = [gen_fn | allowFns]
        simples = [ simple "Bool"
                  , simple "Int"
                  ]
        -- TODO: I now assume all type vars are of kind *, but I should check
        -- this in findTypeVars and return like (HashMap String Int) there!
        tpVars = return . tyVar <$> typeVars
        monos = [ mono "[]"
                ]
        simple = return . tyCon
        mono str = do
            tp <- f typeVars tyVarCount
            return $ tyApp str tp
        tpVar = return $ tyVar tyVarName
        tyVarName = "t" ++ show tyVarCount  -- TODO: make this random?
        gen_fn = randomFnType allowAbstract allowFns nestLimit typeVars tyVarCount

-- | randomly generate a function type
-- TODO: ensure each type var is used at least twice
randomFnType :: Bool -> Bool -> Int -> [String] -> Int -> IO Tp
randomFnType allowAbstract allowFns nestLimit typeVars tyVarCount = do
    let f = randomType allowAbstract allowFns (nestLimit - 1)
    tpIn <- f typeVars tyVarCount
    let typeVarsIn = findTypeVars tpIn
    let typeVars_ = nub $ typeVars ++ typeVarsIn
    tpOut <- f typeVars_ tyVarCount
    return $ TyFun l tpIn tpOut

-- | extract the input and output types from a function type
-- TODO: Maybe
fnTypeIO :: Tp -> (Tp, Tp)
fnTypeIO = \case
    TyFun _l a b -> (a, b)
    -- x -> fail $ "unexpected " ++ show x

-- | this function takes an explicitly typed hole, returning its type
-- TODO: Maybe
holeType :: Expr -> Tp
holeType = \case
    ExpTypeSig _l _exp tp -> tp

-- TODO: c.f. https://hackage.haskell.org/package/ghc-8.6.5/docs/TcHsSyn.html#v:zonkTcTypeToType
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
instantiateTypeVars tps ks = fromList . zip ks <$> sequence (replicate (length ks) tps)

-- | substitute all type variable occurrences
fillTypeVars :: Tp -> HashMap String Tp -> Tp
fillTypeVars tp substitutions = let f = flip fillTypeVars substitutions in case tp of
    TyVar _l _name -> substitutions ! prettyPrint tp
    TyApp _l a b -> TyApp _l (f a) $ f b
    TyFun _l a b -> TyFun _l (f a) $ f b
    _ -> tp

-- | generate a number of concrete types to be used in type variable substitution
-- TODO: move the flatten/nub in
genTypes :: Int -> Int -> IO (Item Tp)
genTypes nestLimit maxInstances = Many . fmap (One . pure) <$> replicateM maxInstances (randomType False False nestLimit [] 0)

-- | dummy source span info, because I don't care
l :: SrcSpanInfo
l = SrcSpanInfo {srcInfoSpan = spn, srcInfoPoints = []}
    where
        spn = SrcSpan "<unknown>.hs" 1 1 1 1

-- | create a qname node
qName :: String -> QName SrcSpanInfo
qName = UnQual l . Ident l

-- | create a monomorphic type node
varNode :: String -> Expr
varNode = Var l . qName

-- | create a monomorphic type node
tyCon :: String -> Tp
tyCon = TyCon l . qName

-- | create a type variable node
tyVar :: String -> Tp
tyVar = TyVar l . Ident l

-- | create a polymorphic type node
tyApp :: String -> Tp -> Tp
tyApp = TyApp l . tyCon
