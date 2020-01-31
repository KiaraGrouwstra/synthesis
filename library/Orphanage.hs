{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | defined type class instances
module Orphanage () where

import Data.Hashable (Hashable(..))
import Data.HashMap.Lazy (HashMap, empty, toList)
import Data.Text.Prettyprint.Doc (Pretty(..), pretty, (<+>), (<>), viaShow, unsafeViaShow, colon, punctuate)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Data.ByteString.Char8 (pack)
import Language.Haskell.Exts.Syntax (
    -- * Modules
    Module, ModuleHead, WarningText, ExportSpecList, ExportSpec, EWildcard,
    ImportDecl, ImportSpecList, ImportSpec, Assoc, Namespace,
    -- * Declarations
    Decl, DeclHead, InstRule, InstHead, Binds, IPBind, PatternSynDirection, InjectivityInfo, ResultSig,
    -- ** Type classes and instances
    ClassDecl, InstDecl, Deriving, DerivStrategy,
    -- ** Data type declarations
    DataOrNew, ConDecl, FieldDecl, QualConDecl, GadtDecl, BangType, Unpackedness,
    -- ** Function bindings
    Match, Rhs, GuardedRhs,
    -- * Class Assertions and Contexts
    Context, FunDep, Asst,
    -- * Types
    Type, Boxed, TyVarBind, Promoted,
    TypeEqn ,
    -- * Expressions
    Exp, Stmt, QualStmt, FieldUpdate, Alt, XAttr,
    -- * Patterns
    Pat, PatField, PXAttr, RPat, RPatOp,
    -- * Literals
    Literal, Sign,
    -- * Variables, Constructors and Operators
    ModuleName, QName, Name, QOp, Op, SpecialCon,
    CName, IPName, XName, Role, MaybePromotedName,
    -- * Template Haskell
    Bracket, Splice,
    -- * FFI
    Safety, CallConv,
    -- * Pragmas
    ModulePragma, Tool, Overlap,
    Rule, RuleVar, Activation,
    Annotation, BooleanFormula
    )

-- | ensure I can use expressions as keys in HashMaps
instance Hashable (Exp l) where
    hashWithSalt salt = hashWithSalt salt . pack . prettyPrint

-- | ensure I can use types as keys in HashMaps
instance Hashable (Type l) where
    hashWithSalt salt = hashWithSalt salt . pack . prettyPrint

instance Pretty (Module l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ModuleHead l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ExportSpecList l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ExportSpec l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ImportDecl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ImportSpecList l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ImportSpec l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Assoc l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Namespace l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Decl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (DeclHead l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (InstRule l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (InstHead l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (IPBind l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (InjectivityInfo l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ResultSig l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ClassDecl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (InstDecl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Deriving l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (DerivStrategy l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (DataOrNew l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ConDecl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (FieldDecl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (QualConDecl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (GadtDecl l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (BangType l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Unpackedness l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Match l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Rhs l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (GuardedRhs l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Context l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (FunDep l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Asst l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Type l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (TyVarBind l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Promoted l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (TypeEqn l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Exp l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Stmt l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (QualStmt l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (FieldUpdate l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Alt l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (XAttr l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Pat l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (PatField l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (PXAttr l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (RPat l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (RPatOp l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Literal l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ModuleName l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (QName l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Name l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (QOp l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Op l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (SpecialCon l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (CName l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (IPName l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (XName l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Role l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (MaybePromotedName l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Bracket l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Splice l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Safety l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (CallConv l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (ModulePragma l) where pretty = unsafeViaShow . prettyPrint
instance Pretty Tool where pretty = unsafeViaShow . prettyPrint
instance Pretty (Overlap l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Rule l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (RuleVar l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Activation l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (Annotation l) where pretty = unsafeViaShow . prettyPrint
instance Pretty (BooleanFormula l) where pretty = unsafeViaShow . prettyPrint

-- | ensure I can print lists
instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
    pretty = viaShow . fmap (\(k,v) -> punctuate colon [pretty k, pretty v]) . toList
