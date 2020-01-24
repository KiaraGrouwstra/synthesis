-- TODO: I guess refactor this to something string-based to get similar types
module Blocks (fnBodies) where

-- | building blocks. warning: we *must* alias existing functions, or their definitions will be regarded as recursive!
fnBodies :: HashMap String String

-- -- | functions used for testing
-- fnBodies = insert "not__" "\\b -> not b" $
-- -- fnBodies = insert "not_" "\\b -> not b :: Bool -> Bool" $
-- -- fnBodies = insert "not_" "\\(b :: Bool) -> not b" $
-- -- fnBodies = insert "not_" "\\b -> let _b = (b :: Bool) in not b" $
-- -- alternative to ScopedTypeVariables: https://stackoverflow.com/q/14540704/1502035
--                 insert "not_" "not" $
--                 -- insert "(.)" "(.)" $ -- TODO: combinators error, cannot generate input samples of type function
--                 insert "id_" "id"  -- TODO: only allow curried version of this function -- applying makes it redundant
--                 empty
fnBodies = fnBodiesTamandu

-- https://raw.githubusercontent.com/shasfin/ml4fp2016/master/baseline/zil/src/builtin.ml
-- https://raw.githubusercontent.com/shasfin/ml4fp2016/master/baseline/zil/src/b_library.tm
fnBodiesTamandu :: HashMap String String
fnBodiesTamandu =
           insert "const_" "const" $
           insert "flip_" "flip" $
           insert "true" "True" $
           insert "false" "False" $
           insert "not_" "not" $
           insert "zero" "0" $
           insert "succ_" "succ" $
           insert "isZero" "(== 0)" $
        --    insert "foldNat" "\\f -> foldNatNat (const f)" $ -- test!
           insert "foldNat" "\\ f acc i -> foldr (const f) acc [1..i]" $ -- test!
        -- https://hackage.haskell.org/package/hmatrix-0.20.0.0/docs/src/Internal.Vector.html#local-6989586621679046393
        -- foldLoop :: (Int -> t -> t) -> t -> Int -> t
        --    insert "foldNatNat" "\\ f s0 d -> go (d - 1) s0; where; go 0 s = f (0::Int) s; go !j !s = go (j - 1) (f j s)" $
           insert "foldNatNat" "\\ f acc i -> foldr f acc [1..i]" $ -- test!
           insert "add" "(+)" $
           insert "mul" "(*)" $
           insert "div_" "div" $ -- Int: for fractions use (/)
           insert "max_" "max" $
           insert "eq" "(==)" $ -- Tamandu restricts this to Int
           insert "neq" "(/=)" $ -- Tamandu restricts this to Int
           insert "nil" "[]" $
           insert "con" "(:)" $
           insert "head_" "head" $
           insert "tail_" "tail" $
           insert "isNil" "null" $
           insert "map_" "map" $  -- list-specific, unlike fmap
           insert "foldr_" "foldr" $
           insert "foldl_" "foldl" $
           insert "filter_" "filter" $
           insert "length_" "length" $
           insert "append" "(++)" $
           insert "reverse_" "reverse" $
           insert "replicate_" "replicate" $
           insert "concat_" "concat" $
           insert "sum_" "sum" $
           insert "prod" "product" $
           insert "maximum_" "maximum" $
           insert "member" "elem" $
           insert "enumTo" "\\i -> [1..i]" $ -- test!
           singleton "enumFromTo" "\\i j -> [i..j]" -- test!

-- | task functions specifically aimed at trying my synthesizers on another paper's algorithm.
-- | the point here is to ensure I'd put these in the test set, while deduping any equivalent functions out of my training set.
-- | I'm now leaning toward instead comparing to other papers by running them on my dataset instead of me running on theirs though.
-- | in that case I should no longer need this anymore.
tasks :: HashMap String String
tasks = tasksTamandu

-- | functionality implemented from: https://raw.githubusercontent.com/shasfin/ml4fp2016/master/baseline/zil/src/benchmark.ml
tasksTamandu :: HashMap String String
tasksTamandu = 
        insert "drop" "drop" $
        insert "droplast" "init" $
        insert "dropmax" "\xs -> filter (maximum xs /=) xs" $
        insert "factorial" "product . flip take [1..]" $ -- https://stackoverflow.com/a/36735375/1502035
        insert "isEven" "\\i -> mod i 2 == (0 :: Int)" $
        insert "last_" "last" $
        insert "mapAdd" "\\n -> map (+ n)" $
        insert "mapDouble" "map (* (2 :: Int))" $
        insert "multfirst" "\\xs -> replicate (length xs) (head xs)" $ -- solution from paper
        insert "multlast" "\\xs -> replicate (length xs) (last xs)" $
        insert "nth" "(!!)" $
        insert "stutter" "concatMap (replicate 2)" $
        -- TODO: the below task functions are also blocks... I guess for the task versions the identically named blocks must specifically be excluded then?
        insert "append" "(++)" $
        insert "concat_" "concat" $
        insert "enumFromTo" "\\i j -> [i..j]" $ -- test!
        insert "enumTo" "\\i -> [1..i]" $ -- test!
        insert "isNil" "null" $
        insert "length_" "length" $
        insert "maximum_" "maximum" $
        insert "member" "elem" $
        insert "replicate_" "replicate" $
        insert "reverse_" "reverse" $
        singleton "sum_" "sum"
