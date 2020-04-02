
-- | utility functions
module Synthesis.Utility
  ( module Synthesis.Utility
    -- Item (..),
    -- NestedTuple (..),
    -- flatten,
    -- pick,
    -- mapKeys,
    -- groupByVal,
    -- fromKeys,
    -- fromVals,
    -- flattenTuple,
    -- mapTuple,
    -- mapTuple3,
    -- tuplify3,
    -- untuple3,
    -- while,
    -- pp,
    -- pp_,
    -- pickKeys,
    -- composeSetters,
    -- fisherYates,
    -- randomSplit,
    -- flipOrder,
    -- equating,
    -- fromKeysM,
    -- fromValsM,
    -- ppMap,
    -- filterHmM,
    -- pickKeysSafe,
    -- nest,
    -- batchList,
    -- statistic,
    -- statistic',
    -- Statistic,
    -- stat,
    -- pipe,
  ) where

import Control.Arrow ((***))
import Control.Monad (filterM, join, foldM)
import Data.Bifoldable (biList)
import Data.List (replicate)
import Data.Bifunctor (first)
import Data.HashMap.Lazy ((!), HashMap, fromList, toList)
import qualified Data.HashMap.Lazy as HM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Hashable (Hashable)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text.Prettyprint.Doc as PP
import GHC.Exts (groupWith)
import Language.Haskell.Exts.Pretty (Pretty, prettyPrint)
import System.Random (RandomGen(..), randomR, randomRIO)

-- | map over both elements of a tuple
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

-- | map over of a 3-element tuple
mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (a1, a2, a3) = (f a1, f a2, f a3)

-- -- | map over of a 4-element tuple
-- mapTuple4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
-- mapTuple4 f (a1, a2, a3, a4) = (f a1, f a2, f a3, f a4)

-- | convert a list of 3(+) items to a tuple of 3
tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)
tuplify3 _ = error "tuplify3 requires list to have 3 elements!"

-- | unpack a tuple into a list
untuple3 :: (a, a, a) -> [a]
untuple3 (x, y, z) = [x, y, z]

-- | a homogeneous nested list
data Item a = One [a] | Many [Item a]

-- | flatten a nested list
flatten :: Item a -> [a]
flatten (One x) = x
flatten (Many x) = concatMap flatten x

-- | a homogeneous nested tuple
data NestedTuple a = SingleTuple (a, a) | DeepTuple (a, NestedTuple a)

-- -- | flatten a nested tuple
-- -- | deprecated, not in use
-- flattenTuple :: NestedTuple a -> [a]
-- flattenTuple = \case
--   SingleTuple tpl -> biList tpl
--   DeepTuple (x, xs) -> x : flattenTuple xs

-- | randomly pick an item from a list
pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- -- | map over the keys of a hashmap
-- -- | deprecated, not in use
-- mapKeys :: (Hashable k, Eq k, Hashable k_, Eq k_) => (k -> k_) -> HashMap k v -> HashMap k_ v
-- mapKeys fn = fromList . fmap (first fn) . toList

-- | group a list of k/v pairs by values, essentially inverting the original HashMap
groupByVal :: (Hashable v, Ord v) => [(k, v)] -> HashMap v [k]
groupByVal = fromList . fmap (\pairs -> (snd $ head pairs, fst <$> pairs)) . groupWith snd

-- | create a HashMap by mapping over a list of keys
fromKeys :: (Hashable k, Eq k) => (k -> v) -> [k] -> HashMap k v
fromKeys fn ks = fromList . zip ks $ fn <$> ks

-- | create a monadic HashMap by mapping over a list of keys
fromKeysM :: (Monad m, Hashable k, Eq k) => (k -> m v) -> [k] -> m (HashMap k v)
fromKeysM fn ks = sequence . fromList . zip ks $ fn <$> ks

-- -- | create a HashMap by mapping over a list of values
-- -- | deprecated, not in use
-- fromVals :: (Hashable k, Eq k) => (v -> k) -> [v] -> HashMap k v
-- fromVals fn vs = fromList $ zip (fn <$> vs) vs

-- | create a monadic HashMap by mapping over a list of values
fromValsM :: (Monad m, Hashable k, Eq k) => (v -> m k) -> [v] -> m (HashMap k v)
fromValsM fn vs = do
  ks <- sequence $ fn <$> vs
  return $ fromList $ zip ks vs

-- filter a HashMap by a monadic predicate

-- -- | deprecated, not in use
-- filterHmM :: (Monad m, Hashable k, Eq k) => ((k, v) -> m Bool) -> HashMap k v -> m (HashMap k v)
-- filterHmM p = fmap fromList . filterM p . toList

-- | while a predicate holds, perform a monadic operation starting from an initial value
while :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
while praed funktion x
  | praed x = do
    y <- funktion x
    while praed funktion y
  | otherwise = return x

-- | shorthand for pretty-printing AST nodes, used for comparisons
pp :: Pretty a => a -> String
pp = prettyPrint

-- | shorthand for pretty-printing AST nodes, used in my top-level module
pp_ :: PP.Pretty a => a -> String
pp_ = show . PP.pretty

-- -- | `show` drop-in for HashMap with Pretty keys.
-- -- | deprecated, not in use
-- ppMap :: (Pretty k, Show v) => HashMap k v -> String
-- ppMap = show . fmap (first pp) . toList

-- | pick some keys from a hashmap
pickKeys :: (Hashable k, Eq k) => [k] -> HashMap k v -> HashMap k v
pickKeys ks hashmap = fromKeys (hashmap !) ks

-- | pick some keys from a hashmap
pickKeysSafe :: (Hashable k, Eq k) => [k] -> HashMap k v -> HashMap k v
pickKeysSafe ks hashmap = fmap fromJust . HM.filter isJust $ fromKeys (`HM.lookup` hashmap) ks

-- | compose two setter functions, as I didn't figure out lenses and their monad instantiations
composeSetters :: (s -> s -> s_) -> (s -> t) -> (t -> b -> s) -> s -> b -> s_
composeSetters newStr newGtr oldStr v part = newStr v $ oldStr (newGtr v) part

-- | helper for fisherYates
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

-- | randomly shuffle a list
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen' = (Map.singleton 0 x, gen')

-- | shuffle a list and split it into sub-lists of specified lengths
splitPlaces :: RandomGen g => g -> [Int] -> [e] -> ([[e]], g)
splitPlaces gen ns xs = (zs_, gen')
  where (zs, gen') = fisherYates gen xs
        zs_ = fst $ foldl (\ (splits, xs_) n -> (splits ++ [take n xs_], drop n xs_)) ([], zs) ns

-- | randomly split a dataset into subsets based on the indicated split ratio
randomSplit :: RandomGen g => g -> (Double, Double, Double) -> [a] -> ([a], [a], [a])
randomSplit gen splits xs =
  let n :: Int = length xs
      ns :: (Int, Int, Int) = mapTuple3 (round . (fromIntegral n *)) splits
   in tuplify3 $ fst $ splitPlaces gen (untuple3 ns) xs

-- -- | flip an Ordering
-- -- | deprecated, not in use
-- flipOrder :: Ordering -> Ordering
-- flipOrder GT = LT
-- flipOrder LT = GT
-- flipOrder EQ = EQ

-- | mapped equality check
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

-- | monadic version of nTimes
nest :: (Monad m) => Int -> (a -> m a) -> a -> m a
nest n f x0 = foldM (\x () -> f x) x0 (replicate n ())

-- -- | split a list into batches of a given size (or less for the last batch)
-- -- | deprecated, not in use
-- batchList :: Int -> [a] -> [[a]]
-- batchList n xs = let
--   batchList' n batches xs =
--     case rest of
--       [] -> batches'
--       _  -> batchList' n batches' rest
--     where
--       (batch, rest) = splitAt n xs
--       batches' = batches <> [batch]
--   in batchList' n [] xs

-- -- is there a point in this if statistics already constitute most Prelude / HaskTorch functions?
-- -- | calculate a statistic for a dataset given an accumulator, a sufficient statistic (fold fn), and a summarizer
-- -- | deprecated, not in use
-- statistic :: Foldable f => b -> (a -> b -> b) -> (f a -> b -> c) -> f a -> c
-- statistic acc sufficientStatistic summarizer xs =
--         summarizer xs $ foldr sufficientStatistic acc xs

-- -- | calculate a statistic for a dataset given a Statistic
-- -- | deprecated, not in use
-- statistic' :: Foldable f => Statistic f a b c -> f a -> c
-- statistic' st xs =
--         (summarizer st) xs $ foldr (sufficientStatistic st) (acc st) xs

-- -- | an (associative) statistic measure compatible with online calculation. this generalizes map-reduce to potentially lower memory complexity as sufficient statistics ensure we may just maintain an accumulator rather than a full mapped data structure.
-- -- | deprecated, not in use
-- data Statistic f a b c = Statistic
--     { acc                 ::  b
--     , sufficientStatistic ::  a  -> b -> b
--     , summarizer          :: Foldable f => f a -> b -> c
--     }

-- -- | default statistic to override
-- -- | deprecated, not in use
-- stat = Statistic
--     { acc = undefined
--     , sufficientStatistic = flip const id
--     , summarizer = const id
--     }

-- | flipped composition
pipe :: (a -> b) -> (b -> c) -> a -> c
pipe = flip (.)
