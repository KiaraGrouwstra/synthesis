{-# LANGUAGE LambdaCase #-}

-- | utility functions specifically related to types
module Types (Tp, Expr, Hole, randomType, randomFnType, tyCon, tyApp, fnTypeIO, genTypes, holeType, var, tyVar, qName, l, findTypeVars, fillTypeVars, star, wildcard, expTypeSig, tyFun, letIn, app, parseExpr, parseType, undef, cxTuple, classA, tyForall, mergeTyVars, unParseResult, unit, symbol, pvar, paren, infixApp, dollar, dot) where

import Language.Haskell.Exts.Syntax ( Exp(..), SpecialCon(..), Type(..), Name(..), QName(..), Type(..), Boxed(..), Binds(..), Decl(..), Rhs(..), Pat(..), TyVarBind(..), Context(..), Asst(..), QOp(..) )
import Language.Haskell.Exts.Parser ( ParseResult(..), ParseMode(..), parse, parseWithMode, fromParseResult, defaultParseMode )
import Language.Haskell.Exts.SrcLoc ( SrcSpan(..), SrcSpanInfo(..), srcInfoSpan, srcInfoPoints )
import Language.Haskell.Exts.Extension ( Extension(..), KnownExtension(..) )
import Data.List (replicate, nub)
import Data.Maybe (fromMaybe)
import Control.Monad (join, replicateM, sequence, filterM)
import Data.HashMap.Lazy (HashMap, empty, fromList, fromListWith, toList, (!), unionWith, keys, elems)
import Utility (Item(..), pick, pp)

-- these verbose types annoy me so let's alias them
type Tp = Type SrcSpanInfo
type Expr = Exp SrcSpanInfo
type Hole = SpecialCon SrcSpanInfo -- ExprHole
-- type Fn = TyFun SrcSpanInfo (Type SrcSpanInfo a) (Type SrcSpanInfo b)

-- | randomly generate a type
-- TODO: allow generating new type vars
randomType :: Bool -> Bool -> Int -> HashMap String [Tp] -> Int -> IO Tp
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
        tpVars = return . tyVar <$> keys typeVars
        monos = [ mono "[]"
                ]
        simple = return . tyCon
        mono str = do
            tp <- f typeVars tyVarCount
            return $ tyApp (tyCon str) tp
        tpVar = return $ tyVar tyVarName
        tyVarName = "t" ++ show tyVarCount  -- TODO: make this random?
        gen_fn = randomFnType allowAbstract allowFns nestLimit typeVars tyVarCount

-- | randomly generate a function type
-- TODO: ensure each type var is used at least twice
randomFnType :: Bool -> Bool -> Int -> HashMap String [Tp] -> Int -> IO Tp
randomFnType allowAbstract allowFns nestLimit typeVars tyVarCount = do
    let f = randomType allowAbstract allowFns (nestLimit - 1)
    tpIn <- f typeVars tyVarCount
    let typeVarsIn = findTypeVars tpIn
    let typeVars_ = mergeTyVars typeVars typeVarsIn
    tpOut <- f typeVars_ tyVarCount
    return $ TyFun l tpIn tpOut

-- merge two maps of type variables and their corresponding type constraints
mergeTyVars :: HashMap String [Tp] -> HashMap String [Tp] -> HashMap String [Tp]
mergeTyVars = unionWith $ \a b -> nub $ a ++ b

-- | extract the input and output types from a function type
-- TODO: Maybe
fnTypeIO :: Tp -> (Tp, Tp)
fnTypeIO = \case
    TyForall _l _maybeTyVarBinds _maybeContext typ -> case typ of
        TyFun _l a b -> (a, b)
    TyFun _l a b -> (a, b)
    -- tp -> (TyTuple l Boxed [], tp)
    -- x -> fail $ "unexpected " ++ show x

-- | this function takes an explicitly typed hole, returning its type
-- TODO: Maybe
holeType :: Expr -> Tp
holeType = \case
    ExpTypeSig _l _exp tp -> tp

-- | find the type variables and their occurrences
findTypeVars :: Tp -> HashMap String [Tp]
findTypeVars = fromListWith (++) . findTypeVars_

findTypeVars_ :: Tp -> [(String, [Tp])]
findTypeVars_ tp = let f = findTypeVars_ in case tp of
            TyVar _l _name -> [(pp tp, [])]
            TyApp _l a b -> f a ++ f b
            TyForall _l maybeTyVarBinds _maybeContext typ -> bindings ++ case typ of
                TyFun _l a b -> f a ++ f b
                where
                    bindings = toList $ fromListWith (++) $ (\(KindedVar _l name kind) -> (pp name, [kind])) <$> fromMaybe [] maybeTyVarBinds
            TyFun _l a b -> f a ++ f b
            _ -> []

-- | substitute all type variable occurrences
fillTypeVars :: Tp -> HashMap String Tp -> Tp
fillTypeVars tp substitutions = let f = flip fillTypeVars substitutions in case tp of
    TyVar _l _name -> substitutions ! pp tp
    TyApp _l a b -> tyApp (f a) $ f b
    TyForall _l _maybeTyVarBinds _maybeContext typ -> case typ of
        TyFun _l a b -> tyFun (f a) $ f b
    TyFun _l a b -> tyFun (f a) $ f b
    _ -> tp

-- | generate a number of concrete types to be used in type variable substitution
-- TODO: move the flatten/nub in
genTypes :: Int -> Int -> IO (Item Tp)
genTypes nestLimit maxInstances = Many . fmap (One . pure) <$> replicateM maxInstances (randomType False False nestLimit empty 0)

-- | dummy source span info, because I don't care
l :: SrcSpanInfo
l = SrcSpanInfo {srcInfoSpan = spn, srcInfoPoints = []}
    where
        spn = SrcSpan "<unknown>.hs" 1 1 1 1

-- | create a typed expression without value, intended for checking types
undef :: Tp -> Expr
undef = expTypeSig (var "undefined")

-- | create a qname node
qName :: String -> QName SrcSpanInfo
qName = UnQual l . Ident l

-- | create a variable node
var :: String -> Expr
var = Var l . qName

-- | $
dollar :: QOp SrcSpanInfo
dollar = symbol "$"

-- | .
dot :: QOp SrcSpanInfo
dot = symbol "."

-- | create a monomorphic type node
tyCon :: String -> Tp
tyCon = TyCon l . qName

-- | create a type variable node
tyVar :: String -> Tp
tyVar = TyVar l . Ident l

-- | create a polymorphic type node
tyApp :: Tp -> Tp -> Tp
tyApp = TyApp l

-- | annotate an expression node with a type signature
expTypeSig :: Expr -> Tp -> Expr
expTypeSig = ExpTypeSig l

-- | type for a function
tyFun :: Tp -> Tp -> Tp
tyFun = TyFun l

tyForall :: Maybe [TyVarBind SrcSpanInfo] -> Maybe (Context SrcSpanInfo) -> Tp -> Tp
tyForall = TyForall l

-- | star type node: *
star :: Tp
star = TyStar l

-- | wildcard type node: _
wildcard :: Tp
wildcard = TyWildCard l Nothing

-- | unit type: ()
unit :: Tp
unit = TyCon l $ Special l $ UnitCon l

-- letIn :: Binds SrcSpanInfo -> Expr -> Expr
-- letIn = Let l
letIn :: HashMap String Expr -> Expr -> Expr
letIn = Let l . binds

-- binds :: [Decl SrcSpanInfo] -> Binds SrcSpanInfo
-- binds = BDecls l
binds :: HashMap String Expr -> Binds SrcSpanInfo
binds = BDecls l . fmap (uncurry patBind) . toList

-- patBind :: Pat SrcSpanInfo -> Rhs SrcSpanInfo -> Maybe (Binds SrcSpanInfo) -> Decl SrcSpanInfo
patBind :: String -> Expr -> Decl SrcSpanInfo
patBind name expr = PatBind l (pvar name) (rhs expr) Nothing

rhs :: Expr -> Rhs SrcSpanInfo
rhs = UnGuardedRhs l

-- pvar :: Name SrcSpanInfo -> Pat SrcSpanInfo
pvar :: String -> Pat SrcSpanInfo
pvar = PVar l . ident

-- | symbol for use in infix expressions
symbol :: String -> QOp SrcSpanInfo
symbol = QVarOp l . UnQual l . Symbol l

-- | parenthesized expression
paren :: Expr -> Expr
paren = Paren l

-- | used in name nodes
ident :: String -> Name SrcSpanInfo
ident = Ident l

-- | function application
app :: Expr -> Expr -> Expr
app = App l

-- | tuple of type constraints
cxTuple :: [Asst SrcSpanInfo] -> Context SrcSpanInfo
cxTuple = CxTuple l

-- | type constraint assertion
classA :: QName SrcSpanInfo -> [Tp] -> Asst SrcSpanInfo
classA = ClassA l

-- | infix function application
infixApp :: Expr -> QOp SrcSpanInfo -> Expr -> Expr
infixApp = InfixApp l

-- lit :: Literal SrcSpanInfo -> Expr
-- lit = Lit l

-- assertParseResult :: Either String a -> a
--         tp <- case exprType expr of
--             Right t -> t
--             Left e -> error $ "failed to type-parse expr " ++ pp expr ++ ": " + e

-- unpack a ParseResult into an Either
unParseResult :: ParseResult a -> Either String a
unParseResult = \case
    ParseOk a -> Right a
    ParseFailed _srcLoc str -> Left str

-- | any compiler extensions to use while parsing
parseMode :: ParseMode
parseMode = defaultParseMode {
    extensions = [
        EnableExtension ScopedTypeVariables
        -- EnableExtension FlexibleContexts
    ]
}

-- | parse an expression from a string
parseExpr :: String -> Expr
-- parseExpr = unParseResult . parse
parseExpr s = case unParseResult (parseWithMode parseMode s :: ParseResult Expr) of
            Right t -> t
            Left e -> error $ "failed to parse expr " ++ s ++ ": " ++ e

-- | parse a type from a string
parseType :: String -> Tp
-- parseType = unParseResult . parse
parseType s = case unParseResult (parseWithMode parseMode s :: ParseResult Tp) of
            Right t -> t
            Left e -> error $ "failed to parse type " ++ s ++ ": " ++ e
