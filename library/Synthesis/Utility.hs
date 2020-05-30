
-- | utility functions
module Synthesis.Utility (module Synthesis.Utility) where

import Control.Arrow ((***))
import Control.Monad (filterM, join, foldM)
import Data.Bifoldable (biList)
import Data.List (replicate, intercalate, maximumBy, minimumBy)
import Data.List.Split (splitOn)
import Data.Bifunctor (Bifunctor, bimap, first)
import Data.HashMap.Lazy ((!), HashMap, fromList, toList)
import qualified Data.HashMap.Lazy as HM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Hashable (Hashable)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text.Prettyprint.Doc as PP
import GHC.Exts (groupWith)
import Language.Haskell.Exts.Pretty (Pretty, prettyPrint)
import System.Random (RandomGen(..), mkStdGen, randomR, randomRIO)
import System.Log.Logger

-- | map over both elements of a bifunctor
mapBoth :: Bifunctor p => (a -> b) -> p a a -> p b b
mapBoth f = bimap f f

-- | map over of a 3-element tuple
mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (a1, a2, a3) = (f a1, f a2, f a3)

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

-- | randomly pick an item from a list
pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)

-- | randomly pick an item from a list given a seed
pickG :: Int -> [a] -> a
pickG seed xs = xs !! i where
    g = mkStdGen seed
    (i, _g') = randomR (0, length xs - 1) g

-- | group a list of k/v pairs by values, essentially inverting the original HashMap
groupByVal :: (Hashable v, Ord v) => [(k, v)] -> HashMap v [k]
groupByVal = fromList . fmap (\pairs -> (snd $ head pairs, fst <$> pairs)) . groupWith snd

-- | create a HashMap by mapping over a list of keys
fromKeys :: (Hashable k, Eq k) => (k -> v) -> [k] -> HashMap k v
fromKeys fn ks = fromList . zip ks $ fn <$> ks

-- | create a monadic HashMap by mapping over a list of keys
fromKeysM :: (Monad m, Hashable k, Eq k) => (k -> m v) -> [k] -> m (HashMap k v)
fromKeysM fn ks = sequence . fromList . zip ks $ fn <$> ks

-- | create a monadic HashMap by mapping over a list of values
fromValsM :: (Monad m, Hashable k, Eq k) => (v -> m k) -> [v] -> m (HashMap k v)
fromValsM fn vs = do
  ks <- sequence $ fn <$> vs
  return $ fromList $ zip ks vs

-- | while a predicate holds, perform a monadic operation starting from an initial value
while :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
while praed funktion x
  | praed x = do
    y <- funktion x
    while praed funktion y
  | otherwise = return x

-- | shorthand for pretty-printing AST nodes, used for comparisons
-- | sorry for the short-hand name, taken from Ruby. I use this a *lot* tho.
pp :: Pretty a => a -> String
pp = prettyPrint

-- | shorthand for pretty-printing AST nodes, used in my top-level module
pp_ :: PP.Pretty a => a -> String
pp_ = show . PP.pretty

-- | pick some keys from a hashmap
pickKeys :: (Hashable k, Eq k) => [k] -> HashMap k v -> HashMap k v
pickKeys ks hashmap = fromKeys (hashmap !) ks

-- | pick some keys from a hashmap
pickKeysSafe :: (Hashable k, Eq k) => [k] -> HashMap k v -> HashMap k v
pickKeysSafe ks hashmap = fromJust <.> HM.filter isJust $ fromKeys (`HM.lookup` hashmap) ks

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

-- | mapped equality check
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating p x y = p x == p y

-- | monadic version of nTimes
nest :: (Monad m) => Int -> (a -> m a) -> a -> m a
nest n f x0 = foldM (\x () -> f x) x0 (replicate n ())

-- | flipped composition
pipe :: (a -> b) -> (b -> c) -> a -> c
pipe = flip (.)

-- | compose over a function returning a functor. this is to `(.)` as `(<$>)` is to `($)`.
-- | helps refactor `fmap a . b` to `a <.> b`.
-- | I picked a precedence to tackle the more painful `fmap (a . b) . c` over `a . fmap b . c`.
-- | still trying to figure out if that decision seems sensible,
-- | as it makes this op left-associative unlike the other three.
-- | also see https://hackage.haskell.org/package/yjtools/docs/Control-Applicative-Tools.html
(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
(<.>) a b = fmap a . b
infixl 5 <.>

-- | create a reverse index (elements to indices) from a list
indexList :: (Eq a, Hashable a) => [a] -> HashMap a Int
indexList xs = fromList $ zip xs [0 .. length xs - 1]

-- | replace occurrences of a sub-list
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to = intercalate to . splitOn from

-- | perform multiple sub-list replacements
replacements :: Eq a => [([a],[a])] -> [a] -> [a]
replacements reps lst = foldl (\ x (from,to) -> replace from to x) lst reps

-- | conditionally transform a value
-- | deprecated, not in use
iff :: Bool -> (a -> a) -> (a -> a)
iff cond fn = if cond then fn else id

-- find the maximum by a mapping function to an ordinal value
maxBy :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maxBy fn = maximumBy $ \a b -> compare (fn a) (fn b)

-- find the minimum by a mapping function to an ordinal value
minBy :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minBy fn = minimumBy $ \a b -> compare (fn a) (fn b)

logPriority :: String -> Priority
logPriority lvl = case lvl of
    "debug"     -> DEBUG
    "info"      -> INFO
    "notice"    -> NOTICE
    "warning"   -> WARNING
    "error"     -> ERROR
    "critical"  -> CRITICAL
    "alert"     -> ALERT
    "emergency" -> EMERGENCY
    _ -> error $ "unknown log level: " <> lvl

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)

traverseSnd :: Monad m => (a, m b) -> m (a, b)
traverseSnd (a, m) = do
    b <- m
    return (a, b)

-- map a HashMap as pairs
asPairs :: (Eq k2, Hashable k2) => ((k1, v1) -> (k2, v2)) -> HashMap k1 v1 -> HashMap k2 v2
asPairs f = fromList . fmap f . toList
