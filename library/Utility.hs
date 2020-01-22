{-# LANGUAGE LambdaCase #-}

-- | utility functions
module Utility (Item(..), NestedTuple(..), flatten, pick, groupByVal, toMapBy, flattenTuple, mapTuple, while, pp, pickKeys, composeSetters) where

import Data.Hashable (Hashable)
import System.Random (randomRIO)
import Data.HashMap.Lazy (HashMap, fromList, (!))
import Language.Haskell.Exts.Pretty ( Pretty, prettyPrint )
import GHC.Exts (groupWith)
import Data.Bifoldable (biList)
import Control.Monad (join)
import Control.Arrow ((***))

-- | map over both elements of a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

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
flattenTuple = \case
    SingleTuple tpl -> biList tpl
    DeepTuple (x, xs) -> x : flattenTuple xs

-- | randomly pick an item from a list
pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- | group a list of k/v pairs by values, essentially inverting the original HashMap
groupByVal :: (Hashable v, Ord v) => [(k, v)] -> HashMap v [k]
groupByVal = fromList . fmap (\pairs -> (snd $ head pairs, fst <$> pairs)) . groupWith snd

-- | create a HashMap by mapping over a list of keys
toMapBy :: (Hashable k, Eq k) => [k] -> (k -> v) -> HashMap k v
toMapBy ks fn = fromList $ zip ks $ fn <$> ks

-- -- ‘hashWithSalt’ is not a (visible) method of class ‘Hashable’
-- -- https://github.com/haskell-infra/hackage-trustees/issues/139
-- instance (Type l) => Hashable (Type l) where
--     -- hash = 1
--     hashWithSalt n = hash
--     -- hashWithSalt salt tp = case (unpack $ pp tp) of
--     --         -- https://github.com/tibbe/hashable/blob/cc4ede9bf7821f952eb700a131cf1852d3fd3bcd/Data/Hashable/Class.hs#L641
--     --         T.Text arr off len -> hashByteArrayWithSalt (TA.aBA arr) (off `shiftL` 1) (len `shiftL` 1) salt
--     -- --  :: Int -> a -> Int
--     -- -- hashWithSalt salt tp = pp tp
--     -- -- hashWithSalt = defaultHashWithSalt

while :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
while praed funktion x
    | praed x = do
        y <- funktion x
        while praed funktion y
    | otherwise = return x

-- | shorthand for pretty-printing AST nodes, inspired by Ruby
pp :: Pretty a => a -> String
pp = prettyPrint

-- | pick some keys from a hashmap
pickKeys :: (Hashable k, Eq k) => [k] -> HashMap k v -> HashMap k v
pickKeys ks hashmap = toMapBy ks (hashmap !)

-- | compose two setter functions, as I didn't figure out lenses and their monad instantiations
composeSetters :: (s -> s -> s_) -> (s -> t) -> (t -> b -> s) -> s -> b -> s_
composeSetters newStr newGtr oldStr v part = newStr v $ oldStr (newGtr v) part
