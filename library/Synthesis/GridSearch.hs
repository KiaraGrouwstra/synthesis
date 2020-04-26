{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | grid-search logic
module Synthesis.GridSearch (module Synthesis.GridSearch) where

import GHC.TypeLits
import Data.Proxy

f :: forall n. KnownNat n => Integer
f = natVal $ Proxy @n

main = do
    print $ take 5 (results @0)
  where
    results :: forall n. KnownNat n => [Integer]
    results = f @n : results @(n + 1)
-- prints: [0,1,2,3,4]
