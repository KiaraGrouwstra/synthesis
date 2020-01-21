module Config (Strategy(..), strategy, nestLimit, maxInstances, numInputs, maxWildcardDepth, genMaxHoles) where

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

-- | the maximum level of functions to imagine in a wildcard for function generation
maxWildcardDepth :: Int
maxWildcardDepth = 2

-- | the maximum number of holes to allow in a generated expression
genMaxHoles :: Int
genMaxHoles = 1
