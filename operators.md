
avoid:

    IO / effects -- no good match for PBE
    type-specific ops, e.g. map/Nothing: wanna keep ops reusable to keep things small
    bunch of intra-type ops -- not as amenable for cutting the search space using types
    infinite lists -- circumstantial, as these will crash evaluation unless combined with e.g. take/takeWhile
    specific inputs (e.g. take/!! taking an Int) -- requires a synthesizer to synthesize specific values which we wanna leave out of scope for simplicity
    functions taking predicates -- see previous; don't have many predicates to use with these as those tend to use values e.g. `== 5` / `< 5`
    functions that are variants with a param order less amenable to currying, e.g. foldl/forM
    functions discarding values, e.g. mapM_/void
    functions likely to induce redundancy, clashing with strong supervision, e.g. id
    reduce redundancy, i.e. preferably don't include operators that can already be constructed from existing ones
    preferably reduce the number of functions with similar type signatures

data:

    Bool: -- True/False, not, (||) --/(&&)
    Int: 0 -- /1, 
    Char: -- ?, 

    Maybe: empty/Just, maybe  -- Maybe/List conversions can be achieved using folds
    List: mempty, (:), null/length
        -- head = foldr1 const
    (,): (,), zip/unzip --, fst
        -- snd = foldr1 const
    -- Either: Left/Right, either

    -- Set: mempty, insert --, fromList
    HashMap: mempty, insert --, fromList --, keys/elems
    -- Tree: Node

class:

    -- class Eq a where
    --     (==) :: a -> a -> Bool
    class Enum a where
        succ :: a -> a
        -- pred :: a -> a
        toEnum :: Int -> a
        fromEnum :: a -> Int
        -- enumFrom :: a -> [a]
        -- enumFromThen :: a -> a -> [a]
        -- enumFromTo :: a -> a -> [a]
        -- enumFromThenTo :: a -> a -> a -> [a]
    -- class Eq a => Ord a where
    --     compare :: a -> a -> Ordering
    --     (<) :: a -> a -> Bool
    --     (<=) :: a -> a -> Bool
    --     (>) :: a -> a -> Bool
    --     (>=) :: a -> a -> Bool
    --     max :: a -> a -> a
    --     min :: a -> a -> a
    class Foldable t where
        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldr :: (a -> b -> b) -> b -> t a -> b
        -- foldl :: (b -> a -> b) -> b -> t a -> b
        foldr1 :: (a -> a -> a) -> t a -> a
        -- foldl1 :: (a -> a -> a) -> t a -> a
        elem :: Eq a => a -> t a -> Bool
        -- maximum :: forall a. Ord a => t a -> a
        -- minimum :: forall a. Ord a => t a -> a
        -- sum :: Num a => t a -> a
        -- product :: Num a => t a -> a
        -- and :: Foldable t => t Bool -> Bool
        -- or :: Foldable t => t Bool -> Bool
        -- any :: Foldable t => (a -> Bool) -> t a -> Bool
        -- all :: Foldable t => (a -> Bool) -> t a -> Bool
        -- concat :: Foldable t => t [a] -> [a]
        -- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
    class (Functor t, Foldable t) => Traversable t where
        traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
        sequenceA :: Applicative f => t (f a) -> f (t a)
        mapM :: Monad m => (a -> m b) -> t a -> m (t b)
        sequence :: Monad m => t (m a) -> m (t a)
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
        -- (<$) :: a -> f b -> f a
    class Applicative m => Monad m where
        (>>=) :: forall a b. m a -> (a -> m b) -> m b
        -- (>>) :: forall a b. m a -> m b -> m b
    class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b  --  = liftA2 id
        -- (*>) :: f a -> f b -> f b
        -- (<*) :: f a -> f b -> f a 
    -- class Applicative f => Alternative f where
        -- empty :: f a
        -- (<|>) :: f a -> f a -> f a
        -- some :: f a -> f [a]
        -- many :: f a -> f [a]
    class Semigroup a => Monoid a where
        mempty :: a
        -- mconcat :: [a] -> a  -- = foldr (<>) mempty
    class Semigroup a where
        (<>) :: a -> a -> a
    class Show a where
        show :: a -> [Char] -- String

    class Bifunctor p where
        bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
        first :: (a -> b) -> p a c -> p b c
        -- second :: (b -> c) -> p a b -> p a c
    class Bifoldable p where
        bifold :: Monoid m => p m m -> m
        bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
        bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
        -- bifoldl :: (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c
    class (Bifunctor t, Bifoldable t) => Bitraversable t where
        bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)

fn:

    -- undefined :: a  -- crashes execution
    -- id :: a -> a  -- not using many combinators where this helps, so I fear it'll mostly induce redundancy which esp. sucks with strong supervision
    const :: a -> b -> a  -- functions as head/snd in foldr1
    (.) :: (b -> c) -> (a -> b) -> a -> c  -- as a combinator may mess with my unrolled grammar if the latter function arg takes multiple params, but useful otherwise so just accept that
    -- flip :: (a -> b -> c) -> b -> a -> c -- arg order mostly shouldn't matter much; as a combinator also may mess with my unrolled grammar

parameter functions:

    (<>):
    (a -> b -> b)

    show:
    Monoid m => (a -> m)
    Applicative f => (a -> f b)
    Monad m => (a -> m b)

    Maybe:
    f (a -> b)

