module Config (Strategy(..), strategy, nestLimit, maxInstances, numInputs) where

-- | max number of instantiations to generate for any type containing type variables
maxInstances :: Int
maxInstances = 5  -- may get less after nub filters out duplicate type instances

-- | max number of levels of nesting for generated types
nestLimit :: Int
nestLimit = 0 -- high values make for big logs while debugging...

-- | max number of inputs to generate
numInputs :: Int
numInputs = 10

data Strategy = UseLambdas | UseCurrying

strategy :: Strategy
-- strategy = UseLambdas
strategy = UseCurrying
