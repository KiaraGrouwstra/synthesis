{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Synthesis.Synthesizer.Utility
 (
    module Synthesis.Synthesizer.Utility
--     learningRate,
--     default_optim,
--     Dev,
--     Tnsr,
--     getDevice,
--     asUntyped,
--     asUntyped',
--     -- availableDevices,
--     Dir,
--     Dirs,
--     dirs,
--     Symbols,
--     symbols,
--     Rules,
--     rules,
--     MaxChar,
--     max_char,
--     M,
--     m,
--     -- T,
--     -- t,
--     BatchSize,
--     batchSize,
--     H0,
--     h0,
--     H1,
--     h1,
--     HiddenFeatures0,
--     hiddenFeatures0,
--     HiddenFeatures1,
--     hiddenFeatures1,
--     padRight,
--     unDim,
--     shape',
--     stack',
--     select',
--     rotate,
--     -- rotateT,
--     fnAppNodes,
--     nodeRule,
--     appRule,
--     lookupRule,
--     dslVariants,
--     batchTensor,
--     batchOp,
--     batchStatistic,
--     TensorStatistic,
--     shuffle,
--     -- combinedOutputs,
--     square,
--     scan,
--     cumulative,
--     assertP,
--     softmaxAll,
--     foldLoop,
--     foldLoop_,
--     unravelIdx,
--     indexList,
--     crossEntropy,
--     f_sumDim,
--     setAt,
    ) where

import Prelude hiding (lookup, exp)
-- import qualified Prelude
import GHC.TypeNats (type (+)) -- , KnownNat, Nat, Mod, type (*), type (-)
import System.Random (RandomGen, Random, random)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap, fromList, lookup)
import System.Environment (getEnv)
import Control.Exception (SomeException, try, assert)
import Control.Monad (void, foldM)
-- import GHC.TypeNats (KnownNat)
import Language.Haskell.Interpreter (Interpreter)  -- , lift, liftIO
import Language.Haskell.Exts.Syntax

import Torch.Typed.Aux (natValI)
import Torch.Typed.Tensor hiding (dim)
import Torch.Typed.Functional
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
import qualified Torch.Device                  as D
import qualified Torch.Optim                   as D
-- import qualified Torch.Functional.Internal     as D
import qualified Torch.Functional.Internal     as I
import qualified Torch.Functional              as F

import Synthesis.Data (Expr, Tp)
import Synthesis.Utility (pp, fisherYates) -- , pp_
import Synthesis.Ast (genBlockVariants)
import Synthesis.Hint (exprType)

import System.IO.Unsafe (unsafePerformIO)
import Torch.Internal.Cast
import qualified Torch.Internal.Managed.Native as ATen
-- import qualified Torch.Internal.Managed.Type.Context as ATen

type Dir = 'Bidirectional
type Dirs = NumberOfDirections Dir
dirs :: Int
dirs = natValI @Dirs

-- | learning rate used in ml optimizer
learningRate :: Float
learningRate = 0.001

-- TODO: consider which hyperparams have been / should be shared across networks

-- TODO: switch to Adam
default_optim :: D.GD  -- D.Optimizer D.GD
default_optim = D.GD  -- GD
--  :: Adam momenta1
-- init_enc_optim  = mkAdam 0 0.9 0.999 $ flattenParameters init_enc_model

-- hm, I'm not sure if NSPS counted hole as a symbol, as holes *have* symbols e.g. for me Expression, in which case there'd be nothing left to distinguish for me...
-- data Symbol = Variable | Hole
-- type Symbols = 2
type LhsSymbols = 1 -- just Expression in our lambda-calculus DSL
-- type Symbols = 1 -- 2  -- holes also just get symbol Expression, so nothing left...
-- symbols :: Int
-- symbols = natValI @Symbols
type RhsSymbols = 36  -- Tamandu
type Symbols = LhsSymbols + RhsSymbols

type Rules = 92     -- Tamandu
-- rules :: Int
-- rules = natValI @Rules

-- | actually Char seems in Int range, i.e. [-2^29 .. 2^29-1]... I think I wouldn't need more than ascii tho.
type MaxChar = 256
max_char :: Int
max_char = natValI @MaxChar

-- | number of features for R3NN expansions/symbols. must be an even number for H.
type M = 20
-- m :: Int
-- m = natValI @M

-- -- | T is the maximum string length for any input or output string
-- -- | use dynamically found values instead...
-- type T = 20
-- t :: Int
-- t = natValI @T

-- ensure this does not overlap with SynthesizerConfig's batchsize
type BatchSize = 8
batchSize :: Int
batchSize = natValI @BatchSize

-- used across a few different MLPs -- does sharing make sense?
type H0 = 20 -- ?
type H1 = 20 -- ?
h0 :: Int
h0 = natValI @H0
h1 :: Int
h1 = natValI @H1

-- left/right MLPs
type HiddenFeatures0 = 20 -- ?
hiddenFeatures0 :: Int
hiddenFeatures0 = natValI @HiddenFeatures0
type HiddenFeatures1 = 20 -- ?
hiddenFeatures1 :: Int
hiddenFeatures1 = natValI @HiddenFeatures1

type Dev = '( 'D.CPU, 0)
type Tnsr dims = Tensor Dev 'D.Float dims

getDevice :: IO D.Device
getDevice = do
  deviceStr <- try (getEnv "DEVICE") :: IO (Either SomeException String)
  print $ show deviceStr
  -- DEVICE: getEnv: does not exist (no environment variable)
  return $ case deviceStr of
    Right "cuda:0" -> D.Device D.CUDA 0
    Right "cpu"    -> D.Device D.CPU 0
    _              -> D.Device D.CPU 0

-- cpu = Proxy @'( 'D.CPU, 0)

-- cuda0 = Proxy @'( 'D.CUDA, 0)

-- availableDevices :: [D.Device]
-- availableDevices =
--   let hasCuda = unsafePerformIO $ cast0 ATen.hasCUDA
--   in  [D.Device { D.deviceType = D.CPU, D.deviceIndex = 0 }]
--         <> (if hasCuda
--              then [D.Device { D.deviceType = D.CUDA, D.deviceIndex = 0 }]
--              else mempty
--            )

-- | right-pad a list to a given length
padRight :: a -> Int -> [a] -> [a]
padRight c n xs = xs ++ replicate (n - length xs) c

-- | use an untyped fn on a typed tensor, works tho unsafe
asUntyped :: forall device dtype shape device' dtype' shape'
           . (D.Tensor -> D.Tensor)
          -> Tensor device dtype shape
          -> Tensor device' dtype' shape'
asUntyped f = UnsafeMkTensor . f . toDynamic

-- | run an untyped op on a typed tensor, delaying safety checks to run-time
asUntyped' :: forall device dtype shape device' dtype' shape'
          .  (TensorOptions shape' dtype' device')
          => (D.Tensor -> D.Tensor)
          -> Tensor device dtype shape
          -> Tensor device' dtype' shape'
asUntyped' f tensor = let
        untyped = f . toDynamic $ tensor
        tensor' = UnsafeMkTensor untyped
        check = D.shape untyped == optionsRuntimeShape @shape' @dtype' @device'
        in assert check tensor'

-- | get the run-time shape of a typed tensor
shape' :: Tensor device dtype shape -> [Int]
shape' = D.shape . toDynamic

-- | stack alternative with a nicer argument order
stack' :: Int -> [D.Tensor] -> D.Tensor
stack' = flip I.stack

-- | cast Int to Int64 (i.e. Torch's Long)
asLong :: Int -> Int64
asLong = fromIntegral

-- | `select` alternative that retains the dimension as a 1
-- | I want this as a built-in, see https://github.com/pytorch/pytorch/issues/34788
select' :: D.Tensor -> Int -> Int -> D.Tensor
select' tensor dim idx = D.indexSelect tensor dim $ D.asTensor [asLong idx]

-- TODO: figure out if Torch has a built-in for this
-- | remove the given dimension from a D.Tensor, spreading it out as a list
unDim :: Int -> D.Tensor -> [D.Tensor]
-- unDim dim tensor = D.select tensor dim <$> [0 .. (D.shape tensor !! dim) - 1]    -- keepdim=False
unDim dim tensor = select' tensor dim <$> [0 .. (D.shape tensor !! dim) - 1] -- keepdim=True

rotate :: [Float] -> [[Float]]
rotate r = res
    where
        n :: Int = length r
        n' :: Int = 2 * n - 1
        l :: [Float] = padRight (0.0 :: Float) n' r
        is :: [Int] = [0 .. length l - 1]
        res :: [[Float]] = (\i -> (\j -> l !! (mod (j - i) n') ) <$> is) <$> is

-- rotateT :: forall n device dtype . KnownNat n => Tensor device dtype '[n] -> [[D.Tensor]]
-- rotateT r = let
--         -- n :: Int = shape r !! 0
--         n = natValI @n
--         -- type N' = 2 * n - 1
--         n' :: Int = 2 * n - 1
--         -- type N = 2 -- ??
--         l :: Tensor device dtype '[n + (n - 1)] = constantPadNd1d @'(0, n-1) 0.0 r
--         is :: [Int] = [0 .. n'-1]
--         -- stack :: [Tensor] -> Int -> Tensor
--         res :: [[D.Tensor]] = (\i -> (\j -> D.select (toDynamic l) 0 (mod (j - i) n')) <$> is) <$> is
--         -- stack @0 (t :. HNil)
--         -- res = (\i -> (\j -> select @0 l @(GHC.TypeNats.Mod (j - i) (2 * n - 1)) l) <$> is) <$> is
--     in res

-- | extract an App chain from an App
fnAppNodes :: Expr -> [Expr]
fnAppNodes = \app -> case app of
    Paren _l xpr -> f xpr
    Let _l _binds xpr -> f xpr
    ExpTypeSig _l xpr _tp -> f xpr
    App _l a b -> f a ++ [b]
    Con _l _qname -> [app]
    Var _l _qname -> [app]
    _ -> []
    where f = fnAppNodes

-- | Let R(n) represent the production rule of non-leaf node n∈N.
nodeRule :: Expr -> String
nodeRule = appRule . fnAppNodes

-- | serialize a function application chain as a skeleton
appRule :: [Expr] -> String
appRule = \case
            [] -> error "no fn!"
            fn : args -> unwords $ pp fn : replicate (length args) "_"

lookupRule :: (Eq k, Hashable k, Show k) => HashMap k v -> k -> v
lookupRule hm k = case (lookup k hm) of
    Just x -> x
    Nothing -> error $ "the DSL does not contain rule " ++ show k ++ "!"

dslVariants :: HashMap String Expr -> Interpreter [(String, Expr)]
dslVariants dsl = do
    fn_types :: HashMap String Tp <- exprType `mapM` dsl
    -- liftIO . print $ "fn_types: " ++ pp_ fn_types
    return $ genBlockVariants maxWildcardDepth fn_types
            where maxWildcardDepth = 0  -- no * here

-- | split a (batch-first) tensor into batches (zero-padded for the last one)
batchTensor :: Int -> D.Tensor -> [D.Tensor]
batchTensor batch_size tensor = let
    nDim = 0
    n = D.shape tensor !! nDim
    numIters = (n-1) `div` batch_size
    f :: Int -> D.Tensor = \i -> let
            from :: Int = (i - 1) * batch_size
            to   :: Int =  i * batch_size  - 1
            idxs :: [Int] = [from .. to]
            paddings :: [Int] = replicate (2 * D.dim tensor + 1) 0 <> [batch_size - n]
        in D.indexSelect tensor nDim . F.constantPadNd1d paddings 0.0 . D.asTensor $ asLong <$> idxs
    in f <$> [0 .. numIters]

-- | batch an associative operation for use on tensor batches
batchOp :: forall shape shape'
        .  (Tnsr shape -> Tnsr shape') -- (Tnsr '[n, *] -> Tnsr '[*])
        -> [Tnsr shape] -- Tnsr '[n, *]
        -> Tnsr shape'   -- Tnsr '[*]
batchOp f batches = f . UnsafeMkTensor $ stack' 0 $ toDynamic . f <$> batches

-- batchedOp :: (D.Tensor -> D.Tensor) -> D.Tensor -> D.Tensor
-- batchedOp f = batchOp f . batchTensor

batchStatistic :: (Tnsr shape -> Tnsr '[]) -> (Tnsr '[] -> Tnsr '[]) -> [Tnsr shape] -> Tnsr '[]
batchStatistic sufficientStatistic summarizer =
        summarizer . batchOp sufficientStatistic

-- -- | deprecated, not in use
-- data TensorStatistic a b c = Statistic
--     { sufficient :: D.Tensor -> Tnsr '[]
--     , summarizer :: Tnsr '[] -> Tnsr '[]
--     }

-- | shuffle a tensor in a given dimension
shuffle :: forall g . (RandomGen g) => g -> Int -> D.Tensor -> (g, D.Tensor)
shuffle gen dim tensor = (gen', shuffled)
    where
        n = D.size tensor dim
        idxs = [0 .. n-1]
        (idxs', gen') = fisherYates gen idxs
        shuffled = D.indexSelect tensor dim $ D.asTensor $ asLong <$> idxs'

-- -- combine i/o lists across type instances and take their outputs
-- combinedOutputs :: HashMap [Tp] [(Expr, Either String Expr)] -> [Either String Expr]
-- combinedOutputs type_ios = outputs
--     where
--         let io_pairs :: [(Expr, Either String Expr)] =
--                 join . elems $ type_ios
--         let outputs :: [Either String Expr] = snd <$> io_pairs

-- | square a tensor, for use in mean-square-error loss
square :: Tnsr shape -> Tnsr shape
square = pow (2 :: Int)

-- | cumulative fold
scan :: (Foldable t) => a -> (a -> a -> a) -> t a -> [a]
scan acc f = tail . foldl (\ as a -> as <> [f a (last as)]) [acc]

-- | get cumulative probabilities
cumulative :: (Num a, Foldable t) => t a -> [a]
cumulative = scan 0 (+)

-- | randomly pick an item by relative probabilities (should sum to 1).
categorical :: (RandomGen g, Fractional a, Ord a, Random a, Foldable t) => g -> t a -> Int
categorical gen probs =
    fromJust . findIndex (> x) $ cumulative probs
    where (x, _gen') = random gen

-- | make an assertion thru a predicate
assertP :: (a -> Bool) -> a -> a
assertP pred_fn x = assert (pred_fn x) x

-- | apply a softmax over all dimensions
-- softmaxAll :: Tensor device 'D.Float shape -> Tensor device 'D.Float shape
-- softmaxAll t = divScalar (toFloat $ sumAll e) e
--     where e = exp t
softmaxAll :: D.Tensor -> D.Tensor
softmaxAll t = F.divScalar ((D.asValue $ F.sumAll e) :: Float) e
    where e = F.exp t

-- | loop n-times, retaining state
foldLoop :: forall a b m . (Num a, Enum a, Monad m) => b -> a -> (b -> a -> m b) -> m b
foldLoop x count block = foldM block x ([1 .. count] :: [a])

-- | loop n-times, retaining state then discarding it at the end
foldLoop_ :: forall a b m . (Num a, Enum a, Monad m) => b -> a -> (b -> a -> m b) -> m ()
foldLoop_ = ((void .) .) . foldLoop

-- | like np.unravel_idx, unravel a flat index (from e.g. argmax_t) to the dimensions of a tensor
unravelIdx :: D.Tensor -> Int -> [Int]
unravelIdx t idx = snd . foldr (\ dim_ (idx_, idxs) -> (idx_ `Prelude.div` dim_, idx_ `Prelude.mod` dim_ : idxs)) (idx, []) $ D.shape t

-- | create a reverse index (elements to indices) from a list
indexList :: (Eq a, Hashable a) => [a] -> HashMap a Int
indexList xs = fromList $ zip xs [0 .. length xs - 1]

-- TODO: replace with built-in
-- | calculate the cross-entropy loss given target indices, a class dimension, and a predictions tensor
crossEntropy :: D.Tensor -> Int -> D.Tensor -> D.Tensor
crossEntropy target dim input = F.nllLoss' (F.logSoftmax dim input) target

-- | TODO: replace with actual F.sumDim
f_sumDim :: Int -> D.Tensor -> D.Tensor
f_sumDim dim t = I.sumDim t dim False $ D.dtype t

-- -- | TODO: replace with actual F.squeezeDim
f_squeezeDim :: Int -> D.Tensor -> D.Tensor
f_squeezeDim dim t = I.squeezeDim t dim

-- | 'setAt' sets the element at the index.
-- | If the index is negative or exceeds list length, the original list will be returned.
-- | src: https://hackage.haskell.org/package/ilist-0.4.0.0/docs/src/Data.List.Index.html#setAt
setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_:xs) = a : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

-- TODO: import from hasktorch
f_multinomial_tlb
  :: D.Tensor
  -> Int
  -> Bool
  -> IO D.Tensor
f_multinomial_tlb t l b =
  (cast3 ATen.multinomial_tlb) t l b
