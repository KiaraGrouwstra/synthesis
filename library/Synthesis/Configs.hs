{-# LANGUAGE UnicodeSyntax #-}

module Synthesis.Configs
  ( seed,
    typesByArity,
    nestLimit,
    maxInstances,
    numInputs,
    maxWildcardDepth,
    genMaxHoles,
    split,
    listLengths,
    intRange,
  )
where

import Data.HashMap.Lazy (HashMap, singleton, insert)

-- | random seed
-- | TODO: make this program input
seed :: Int
seed = 123

-- | synthesized types, categorized by arity
typesByArity :: HashMap Int [String]
typesByArity =
  insert 1 ["[]"] $
  singleton 0 ["Bool", "Int"]

-- dataset generation

-- TODO: figure out a sensible split

-- | ratio between the number of samples to use in the training, validation, and test sets, respectively.
split :: (Double, Double, Double)
split = (0.7, 0.2, 0.1)

-- | the range of elements to generate for list types
listLengths :: (Int, Int)
listLengths = (0, 5)

-- | the range of integers to generate
intRange :: (Integer, Integer)
intRange = (-20, 20)

-- type generation

-- | max number of instantiations to generate for any type containing type variables.
-- | may get less after nub filters out duplicate type instances.
maxInstances :: Int
maxInstances = 5

-- | max number of levels of nesting for generated types.
-- | high values make for big logs while debugging...
nestLimit :: Int
nestLimit = 0

-- sample generation

-- | max number of inputs to generate.
-- | may get less after nub filters out duplicates.
numInputs :: Int
numInputs = 10

-- function generation

-- | the maximum level of functions to imagine in a wildcard for function generation
maxWildcardDepth :: Int
maxWildcardDepth = 2

-- | the maximum number of holes to allow in a generated expression
genMaxHoles :: Int
genMaxHoles = 1
