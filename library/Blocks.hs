module Blocks (blockAsts, fnAsts, constants) where

import Types (Expr, parseExpr)
import Data.HashMap.Lazy (HashMap, singleton, insert, union)

blockAsts :: HashMap String Expr
blockAsts = fnAsts `union` constants

-- | building blocks
fnAsts :: HashMap String Expr

-- -- | functions used for testing
-- fnAsts = insert "not_" "\\b -> not b" $
-- -- fnAsts = insert "not" "\\b -> not b :: Bool -> Bool" $
-- -- fnAsts = insert "not" "\\(b :: Bool) -> not b" $
-- -- fnAsts = insert "not" "\\b -> let _b = (b :: Bool) in not b" $
-- -- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
--                 insert "not" "not" $
--                 -- insert "(.)" "(.)" $ -- TODO: combinators error, cannot generate input samples of type function
--                 singleton "id" "id"  -- TODO: only allow curried version of this function -- applying makes it redundant
fnAsts = fnAstsTamandu

-- https://raw.githubusercontent.com/shasfin/ml4fp2016/master/baseline/zil/src/builtin.ml
-- https://raw.githubusercontent.com/shasfin/ml4fp2016/master/baseline/zil/src/b_library.tm
fnAstsTamandu :: HashMap String Expr
fnAstsTamandu = fmap parseExpr $
        --    insert "const" "const" $
           insert "flip" "flip" $
         --   insert "not" "not" $
         --   insert "isZero" "(== 0)" $
        -- --    insert "foldNat" "\\f -> foldNatNat (const f)" $ -- test!
        --    insert "foldNat" "\\ f acc i -> foldr (const f) acc [1..i]" $ -- test!
        -- -- https://hackage.haskell.org/package/hmatrix-0.20.0.0/docs/src/Internal.Vector.html#local-6989586621679046393
        -- -- foldLoop :: (Int -> t -> t) -> t -> Int -> t
        -- --    insert "foldNatNat" "\\ f s0 d -> go (d - 1) s0; where; go 0 s = f (0::Int) s; go !j !s = go (j - 1) (f j s)" $
        --    insert "foldNatNat" "\\ f acc i -> foldr f acc [1..i]" $ -- test!
         --   insert "add" "(+)" $
         --   insert "mul" "(*)" $
         --   insert "div" "div" $ -- Int: for fractions use (/)
         --   insert "max" "max" $
        --    insert "eq" "(==)" $ -- Tamandu restricts this to Int
        --    insert "neq" "(/=)" $ -- Tamandu restricts this to Int
         --   insert "con" "(:)" $
         --   insert "head" "head" $
         --   insert "tail" "tail" $
         --   insert "isNil" "null" $
        --    insert "map" "map" $  -- list-specific, unlike fmap
         --   insert "foldr" "foldr" $
         --   insert "foldl" "foldl" $
        --    insert "filter" "filter" $
         --   insert "length" "length" $
         --   insert "append" "(++)" $
         --   insert "reverse" "reverse" $
         --   insert "replicate" "replicate" $
         --   insert "concat" "concat" $
         --   insert "sum" "sum" $
         --   insert "prod" "product" $
         --   insert "maximum" "maximum" $
         --   insert "member" "elem" $
         --   insert "enumTo" "\\i -> [1..i]" $
         --   insert "enumFromTo" "enumFromTo" $
           singleton "succ" "succ"

constants :: HashMap String Expr
constants = constantsTamandu

_constantsTest :: HashMap String Expr
_constantsTest = parseExpr <$>
                singleton "true" "True"

constantsTamandu :: HashMap String Expr
constantsTamandu = fmap parseExpr $
           insert "nil" "[]" $
           insert "true" "True" $
           insert "false" "False" $
           singleton "zero" "0 :: Int"

-- | task functions specifically aimed at trying my synthesizers on another paper's algorithm.
-- | the point here is to ensure I'd put these in the test set, while deduping any equivalent functions out of my training set.
-- | I'm now leaning toward instead comparing to other papers by running them on my dataset instead of me running on theirs though.
-- | in that case I should no longer need this anymore.
_tasks :: HashMap String String
_tasks = tasksTamandu

-- | functionality implemented from: https://raw.githubusercontent.com/shasfin/ml4fp2016/master/baseline/zil/src/benchmark.ml
tasksTamandu :: HashMap String String
tasksTamandu = 
        insert "drop" "drop" $
        insert "droplast" "init" $
        insert "dropmax" "\\xs -> filter (maximum xs /=) xs" $
        insert "factorial" "product . flip take [1..]" $ -- https://stackoverflow.com/a/36735375/1502035
        insert "isEven" "\\i -> mod i 2 == (0 :: Int)" $
        insert "last" "last" $
        insert "mapAdd" "\\n -> map (+ n)" $
        insert "mapDouble" "map (* (2 :: Int))" $
        insert "multfirst" "\\xs -> replicate (length xs) (head xs)" $ -- solution from paper
        insert "multlast" "\\xs -> replicate (length xs) (last xs)" $
        insert "nth" "(!!)" $
        insert "stutter" "concatMap (replicate 2)" $
        -- TODO: the below task functions are also blocks... I guess for the task versions the identically named blocks must specifically be excluded then?
        insert "append" "(++)" $
        insert "concat" "concat" $
        insert "enumFromTo" "enumFromTo" $
        insert "enumTo" "\\i -> [1..i]" $ -- test!
        insert "isNil" "null" $
        insert "length" "length" $
        insert "maximum" "maximum" $
        insert "member" "elem" $
        insert "replicate" "replicate" $
        insert "reverse" "reverse" $
        singleton "sum" "sum"
