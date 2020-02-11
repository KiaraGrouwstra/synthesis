{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | defined type class instances
module Synthesis.Orphanage
  (
  )
where

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS
import Data.HashMap.Lazy (HashMap, toList)
import Data.Hashable (Hashable (..))
import Data.Store (Store (..), Size (..), Peek (..), Poke (..))
import Data.Store.Internal (genericSize, genericPeek, genericPoke)
import Data.Text.Prettyprint.Doc
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Synthesis.Data

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

-- | ensure I can print hashmaps
instance (Pretty k, Pretty v) => Pretty (HashMap k v) where
  pretty = vcat . fmap (\(k, v) -> indent 2 $ fillSep $ punctuate colon [pretty k, indent 4 $ pretty v]) . toList

-- -- serialization
-- instance (Eq k, Hashable k, Store k, Store a) => Store (HashMap k a) where
--     size = sizeMap
--     poke = pokeMap
--     peek = peekMap

-- size = VarSize $ BS.length . pack . prettyPrint

instance Store l => Store (Module l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ModuleHead l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ExportSpecList l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ExportSpec l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ImportDecl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ImportSpecList l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ImportSpec l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Assoc l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Namespace l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Decl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (DeclHead l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (InstRule l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (InstHead l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (IPBind l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (InjectivityInfo l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ResultSig l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ClassDecl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (InstDecl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Deriving l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (DerivStrategy l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (DataOrNew l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ConDecl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (FieldDecl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (QualConDecl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (GadtDecl l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (BangType l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Unpackedness l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Match l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Rhs l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (GuardedRhs l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Context l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (FunDep l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Asst l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Type l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (TyVarBind l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Promoted l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (TypeEqn l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Exp l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Stmt l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (QualStmt l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (FieldUpdate l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Alt l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (XAttr l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Pat l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (PatField l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (PXAttr l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (RPat l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (RPatOp l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Literal l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ModuleName l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (QName l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Name l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (QOp l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Op l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (SpecialCon l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (CName l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (IPName l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (XName l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Role l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (MaybePromotedName l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Bracket l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Splice l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Safety l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (CallConv l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (ModulePragma l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store Tool where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Overlap l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Rule l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (RuleVar l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Activation l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Annotation l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (BooleanFormula l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Sign l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (Binds l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (EWildcard l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (WarningText l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store l => Store (PatternSynDirection l) where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store Boxed where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store SrcSpanInfo where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store SrcSpan where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

instance Store Stuff where
    size = genericSize
    poke = genericPoke
    peek = genericPeek

-- instance Pretty Stuff where
--   pretty = id
