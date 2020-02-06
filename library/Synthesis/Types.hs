{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | utility functions specifically related to types
module Synthesis.Types
  ( Tp,
    Expr,
    Hole,
    randomType,
    randomFnType,
    tyCon,
    tyApp,
    fnTypeIO,
    genTypes,
    holeType,
    var,
    tyVar,
    qName,
    l,
    findTypeVars,
    fillTypeVars,
    star,
    wildcard,
    expTypeSig,
    tyFun,
    letIn,
    app,
    parseExpr,
    parseType,
    undef,
    cxTuple,
    iParam,
    unIPName,
    typeA,
    unQName,
    unName,
    tyForall,
    mergeTyVars,
    unParseResult,
    unit,
    symbol,
    pvar,
    ptuple,
    paren,
    infixApp,
    dollar,
    dot,
    list,
    tuple,
    int,
    string,
    con,
    lambda,
    tyList,
    tyParen,
    fnInputTypes,
    fnTypes,
    typeSane,
    isFn,
    hasFn,
    nubPp,
    unList,
  )
where

import Control.Monad (join, replicateM)
import Data.Bifunctor (first)
import Data.HashMap.Lazy
  ( (!),
    HashMap,
    empty,
    fromListWith,
    keys,
    toList,
    unionWith,
  )
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Language.Haskell.Exts.Extension
  ( Extension (..),
    KnownExtension (..),
  )
import Language.Haskell.Exts.Parser
  ( ParseMode (..),
    ParseResult (..),
    defaultParseMode,
    parseWithMode,
  )
import Language.Haskell.Exts.Pretty (Pretty)
import Language.Haskell.Exts.SrcLoc
  ( SrcSpan (..),
    SrcSpanInfo (..),
    srcInfoPoints,
    srcInfoSpan,
  )
import Language.Haskell.Exts.Syntax
  ( Asst (..),
    Binds (..),
    Boxed (..),
    Context (..),
    Decl (..),
    Exp (..),
    Literal (..),
    Name (..),
    Pat (..),
    Promoted (..),
    QName (..),
    QOp (..),
    Rhs (..),
    SpecialCon (..),
    TyVarBind (..),
    Type (..),
    IPName (..),
  )
import Synthesis.Orphanage ()
import Synthesis.Utility (Item (..), equating, pick, pp)

-- these verbose types annoy me so let's alias them
type L = SrcSpanInfo

type Tp = Type L

type Expr = Exp L

-- | deprecated, not in use
type Hole = SpecialCon L -- ExprHole
    -- type Fn = TyFun L (Type L a) (Type L b)

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
    simples =
      [ simple "Bool",
        simple "Int"
      ]
    -- TODO: I now assume all type vars are of kind *, but I should check
    -- this in findTypeVars and return like (HashMap String Int) there!
    tpVars = return . tyVar <$> keys typeVars
    monos =
      [ mono "[]"
      ]
    simple = return . tyCon
    mono str = do
      tp <- f typeVars tyVarCount
      return $ tyApp (tyCon str) tp
    tpVar = return $ tyVar tyVarName
    tyVarName = "t" ++ show tyVarCount -- TODO: make this random?
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
mergeTyVars = unionWith $ \a b -> nubPp $ a ++ b

-- | extract the input and output types from a function type
-- TODO: Maybe
fnTypeIO :: Tp -> ([Tp], Tp)
fnTypeIO = \case
  TyForall _l maybeTyVarBinds maybeContext tp -> case tp of
    TyFun _l a b -> first (f a :) $ fnTypeIO $ f b
    typ -> ([], typ)
    where
      f = TyForall _l maybeTyVarBinds maybeContext
  TyFun _l a b -> first (a :) $ fnTypeIO b
  TyParen _l a -> fnTypeIO a
  tp -> ([], tp)

-- | extract the input and output types from a function type as one list
fnTypes :: Tp -> [Tp]
fnTypes = (\tpl -> fst tpl ++ [snd tpl]) . fnTypeIO

-- | extract the input types from a function type
fnInputTypes :: Tp -> [Tp]
fnInputTypes = \case
  TyForall _l maybeTyVarBinds maybeContext typ -> case typ of
    TyFun _l a b -> f a : fnInputTypes (f b)
    _ -> []
    where
      f = TyForall _l maybeTyVarBinds maybeContext
  TyFun _l a b -> a : fnInputTypes b
  _ -> []

-- | this function takes an explicitly typed hole, returning its type
-- | deprecated, not in use
-- TODO: Maybe
holeType :: Expr -> Tp
holeType = \case
  ExpTypeSig _l _exp tp -> tp

-- | find the type variables and their occurrences
findTypeVars :: Tp -> HashMap String [Tp]
findTypeVars = fromListWith (++) . findTypeVars_

-- | recursive `findTypeVars_` helper
findTypeVars_ :: Tp -> [(String, [Tp])]
findTypeVars_ tp =
  let f = findTypeVars_
   in case tp of
        TyForall _l maybeTyVarBinds maybeContext typ -> bindings ++ context ++ f typ
          where
            bindings :: [(String, [Tp])] = toList $ fromListWith (++) $ (\(KindedVar _l name kind) -> (pp name, [kind])) <$> fromMaybe [] maybeTyVarBinds
            context :: [(String, [Tp])] = fromContext $ fromMaybe (CxEmpty l) maybeContext
            fromContext :: Context L -> [(String, [Tp])] = \case
              CxTuple _l assts -> concat $ unAsst <$> assts
              CxSingle _l asst -> unAsst asst
              CxEmpty _l -> []
            unAsst :: Asst L -> [(String, [Tp])] = \case
              TypeA _l typ -> case typ of
                TyApp _l a b -> [(pp b, [a])]
                _ -> f typ
              IParam _l _iPName a -> f a
              ParenA _l asst -> unAsst asst
        TyFun _l a b -> f a ++ f b
        TyTuple _l _boxed tps -> concat $ f <$> tps
        TyUnboxedSum _l tps -> concat $ f <$> tps
        TyList _l a -> f a
        TyParArray _l a -> f a
        TyApp _l a b -> f a ++ f b
        TyVar _l _name -> [(pp tp, [])]
        TyParen _l a -> f a
        TyKind _l a kind -> f a ++ f kind
        TyPromoted _l promoted -> case promoted of
          PromotedList _l _bl tps -> concat $ f <$> tps
          PromotedTuple _l tps -> concat $ f <$> tps
          _ -> []
        TyEquals _l a b -> f a ++ f b
        TyBang _l _bangType _unpackedness a -> f a
        _ -> []

-- | substitute all type variable occurrences
fillTypeVars :: Tp -> HashMap String Tp -> Tp
fillTypeVars tp substitutions =
  let f = flip fillTypeVars substitutions
   in case tp of
        TyForall _l _maybeTyVarBinds _maybeContext a -> f a -- if I'm filling type vars I guess type constraints can be stripped out
        TyFun _l a b -> tyFun (f a) $ f b
        TyTuple _l boxed tps -> TyTuple l boxed $ f <$> tps
        TyUnboxedSum _l tps -> TyUnboxedSum l $ f <$> tps
        TyList _l a -> tyList $ f a
        TyParArray _l a -> TyParArray l $ f a
        TyApp _l a b -> tyApp (f a) $ f b
        TyVar _l _name -> substitutions ! pp tp
        TyParen _l a -> TyParen l $ f a
        TyKind _l a kind -> TyKind l (f a) $ f kind
        TyPromoted _l promoted -> TyPromoted l $ case promoted of
          PromotedList _l bl tps -> PromotedList l bl $ f <$> tps
          PromotedTuple _l tps -> PromotedTuple l $ f <$> tps
          _ -> promoted
        TyEquals _l a b -> TyEquals l (f a) $ f b
        TyBang _l bangType unpackedness a -> TyBang l bangType unpackedness $ f a
        _ -> tp

-- | generate a number of concrete types to be used in type variable substitution
-- TODO: move the flatten/nub in
genTypes :: Int -> Int -> IO (Item Tp)
genTypes nestLimit maxInstances = Many . fmap (One . pure) <$> replicateM maxInstances (randomType False False nestLimit empty 0)

-- | dummy source span info, because I don't care
l :: L
l = SrcSpanInfo {srcInfoSpan = spn, srcInfoPoints = []}
  where
    spn = SrcSpan "<unknown>.hs" 1 1 1 1

-- | create a typed expression without value, intended for checking types
undef :: Tp -> Expr
undef = expTypeSig (var "undefined")

-- | create a qname node
qName :: String -> QName L
qName = UnQual l . Ident l

-- | create a variable node
var :: String -> Expr
var = Var l . qName

-- | \$
dollar :: QOp L
dollar = symbol "$"

-- | .
dot :: QOp L
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

-- | type constraint
tyForall :: Maybe [TyVarBind L] -> Maybe (Context L) -> Tp -> Tp
tyForall = TyForall l

-- | star type node: *
-- | deprecated, not in use
star :: Tp
star = TyStar l

-- | wildcard type node: _
wildcard :: Tp
wildcard = TyWildCard l Nothing

-- | unit type: ()
unit :: Tp
unit = TyCon l $ Special l $ UnitCon l

-- | parenthesized type
tyParen :: Tp -> Tp
tyParen = TyParen l

-- | let-expression
letIn :: HashMap String Expr -> Expr -> Expr
letIn = Let l . binds

-- | variable definitions in e.g. a let-expression
binds :: HashMap String Expr -> Binds L
binds = BDecls l . fmap (uncurry patBind) . toList

-- | variable definition in e.g. a let-expression
-- patBind :: Pat L -> Rhs L -> Maybe (Binds L) -> Decl L
patBind :: String -> Expr -> Decl L
patBind name expr = PatBind l (pvar name) (rhs expr) Nothing

-- | right-hand side of an assignment
rhs :: Expr -> Rhs L
rhs = UnGuardedRhs l

-- | variable name as used on the left-hand side of an assignment
pvar :: String -> Pat L
pvar = PVar l . ident

-- | tuple pattern as used on the left-hand side of an assignment
ptuple :: [Pat L] -> Pat L
ptuple = PTuple l Boxed

-- | symbol for use in infix expressions
symbol :: String -> QOp L
symbol = QVarOp l . UnQual l . Symbol l

-- | parenthesized expression
paren :: Expr -> Expr
paren = Paren l

-- | used in name nodes
ident :: String -> Name L
ident = Ident l

-- | function application
app :: Expr -> Expr -> Expr
app = App l

-- | tuple of type constraints
cxTuple :: [Asst L] -> Context L
cxTuple = CxTuple l

-- | implicit parameter constraint
-- | deprecated, not in use
iParam :: String -> Tp -> Asst L
iParam str = IParam l (IPLin l str)

typeA :: String -> Tp -> Asst L
typeA str tp = TypeA l $ tyApp (tyCon str) tp
-- (tyVar "a")
-- (IPLin l str) tp

-- | get the string from an IPName
-- | deprecated, not in use
unIPName :: IPName L -> String
unIPName = \case
    IPDup _l str -> str -- ^ ?/ident/, non-linear implicit parameter
    IPLin _l str -> str -- ^ %/ident/, linear implicit parameter

-- | get the string from a QName
-- | deprecated, not in use
unQName :: QName L -> String
unQName = \case
    Qual _l _moduleName name -> unName name -- ^ name qualified with a module name
    UnQual _l name -> unName name -- ^ unqualified local name
    Special _l _specialCon -> error "SpecialCon is not string-based"  -- ^ built-in constructor with special syntax

-- | get the string from a Name
unName :: Name L -> String
unName = \case
    Ident _l str -> str   -- ^ /varid/ or /conid/.
    Symbol _l str -> str  -- ^ /varsym/ or /consym/

-- | infix function application
infixApp :: Expr -> QOp L -> Expr -> Expr
infixApp = InfixApp l

-- | a list of expressions
list :: [Expr] -> Expr
list = List l

-- | a tuple of expressions
tuple :: [Expr] -> Expr
tuple = Tuple l Boxed

-- | a literal expression
lit :: Literal L -> Expr
lit = Lit l

-- | Int expression
int :: Integer -> Expr
int i = lit $ Int l i $ show i

-- | String expression
string :: String -> Expr
string s = lit $ String l s s

-- | data constructor
con :: String -> Expr
con = Con l . qName

-- | lambda function
lambda :: [Pat L] -> Expr -> Expr
lambda = Lambda l

-- | list type
tyList :: Tp -> Tp
tyList = TyList l

-- | unpack a list expression
unList :: Expr -> [Expr]
unList = \case
    (List _l exps) -> exps
    _ -> error "expected list"

-- unpack a ParseResult into an Either
unParseResult :: ParseResult a -> Either String a
unParseResult = \case
  ParseOk a -> Right a
  ParseFailed _srcLoc str -> Left str

-- | any compiler extensions to use while parsing
parseMode :: ParseMode
parseMode =
  defaultParseMode
    { extensions =
        [ EnableExtension ScopedTypeVariables,
          EnableExtension ConstraintKinds
          -- , EnableExtension FlexibleContexts
        ]
    }

-- | parse an expression from a string
parseExpr :: String -> Expr
parseExpr s = case unParseResult (parseWithMode parseMode s :: ParseResult Expr) of
  Right t -> t
  Left e -> error $ "failed to parse expr " ++ s ++ ": " ++ e

-- | parse a type from a string
parseType :: String -> Tp
parseType s = case unParseResult (parseWithMode parseMode s :: ParseResult Tp) of
  Right t -> t
  Left e -> error $ "failed to parse type " ++ s ++ ": " ++ e

-- | check if a type is a function type
isFn :: Tp -> Bool
isFn = \case
  TyFun _l _a _b -> True
  TyForall _l _maybeTyVarBinds _maybeContext tp -> isFn tp
  TyParen _l a -> isFn a
  _ -> False

-- | check if a type contains a function type
hasFn :: Tp -> Bool
hasFn typ =
  let f = hasFn
   in case typ of
        TyFun _l _a _b -> True
        TyForall _l _maybeTyVarBinds _maybeContext tp -> f tp
        TyTuple _l _boxed tps -> or $ f <$> tps
        TyUnboxedSum _l tps -> or $ f <$> tps
        TyList _l a -> f a
        TyParArray _l a -> f a
        TyApp _l a b -> f a || f b
        TyParen _l a -> f a
        TyKind _l a kind -> f a || f kind
        TyPromoted _l promoted -> case promoted of
          PromotedList _l _bl tps -> or $ f <$> tps
          PromotedTuple _l tps -> or $ f <$> tps
          _ -> False
        TyEquals _l a b -> f a || f b
        TyBang _l _bangType _unpackedness a -> f a
        _ -> False

-- | check if a type is sane -- basically we wanna throw out crap like list of function
typeSane :: Tp -> Bool
typeSane tp = not (hasFn tp) || (isFn tp && (and (typeSane <$> fnTypes tp)))

-- | filter out duplicate types. note this dedupe will fail for type variable variations...
nubPp :: Pretty a => [a] -> [a]
nubPp = nubBy (equating pp)
