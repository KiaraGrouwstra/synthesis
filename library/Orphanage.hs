{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | defined type class instances
module Orphanage () where

import Data.Hashable (Hashable(..))
import Language.Haskell.Exts.Syntax ( Exp, Type )
import Utility (pp)
import Data.ByteString.Char8 (pack)

-- | ensure I can use expressions as keys in HashMaps
instance Hashable (Exp l) where
    hashWithSalt salt xpr = hashWithSalt salt $ pack . pp $ xpr

-- | ensure I can use types as keys in HashMaps
instance Hashable (Type l) where
    hashWithSalt salt tp = hashWithSalt salt $ pack . pp $ tp
