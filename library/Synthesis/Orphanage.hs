{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | defined type class instances
module Synthesis.Orphanage () where

import Data.ByteString.Char8 (pack)
import Data.HashMap.Lazy (HashMap, toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Aeson
import Data.Hashable (Hashable (..))
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc as PP
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Synthesis.Data
import Synthesis.Utility
import Synthesis.Types

-- | ensure I can use expressions as keys in HashMaps
instance Hashable (Exp l) where
  hashWithSalt salt = hashWithSalt salt . pack . prettyPrint

-- | ensure I can use types as keys in HashMaps
instance Hashable (Type l) where
  hashWithSalt salt = hashWithSalt salt . pack . prettyPrint

instance Hashable HparComb

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

-- | ensure I can print eithers
instance (Pretty l, Pretty r) => Pretty (Either l r) where
  pretty = unsafeViaShow . \case
    Left  l -> PP.pretty ( "Left (" :: String) <> PP.pretty l <> PP.pretty (")" :: String)
    Right r -> PP.pretty ("Right (" :: String) <> PP.pretty r <> PP.pretty (")" :: String)

-- | ensure I can print sets
instance (Pretty a) => Pretty (Set a) where
  pretty = pretty . Set.toList

instance FromJSON Tp where
    parseJSON = withObject "Tp" $ parseType <.> (.: "type")

instance ToJSON Tp where
    toJSON tp = object ["type" .= pp tp]

instance ToJSONKey Tp

instance FromJSONKey Tp

instance FromJSON Expr where
    parseJSON = withObject "Expr" $ parseExpr <.> (.: "expr")

instance ToJSON Expr where
    toJSON expr = object ["expr" .= pp expr]

instance ToJSONKey Expr

instance FromJSONKey Expr

instance FromJSON TaskFnDataset
instance ToJSON TaskFnDataset where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GenerationConfig
instance ToJSON GenerationConfig where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON SynthesizerConfig
instance ToJSON SynthesizerConfig where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON GridSearchConfig
instance ToJSON GridSearchConfig where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON EvolutionaryConfig
instance ToJSON EvolutionaryConfig where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON OptimizationConfig
instance ToJSON OptimizationConfig where
    toEncoding = genericToEncoding defaultOptions
