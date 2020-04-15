{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.Utility (module Synthesis.Synthesizer.Utility) where

import Prelude hiding (lookup, exp)
import GHC.Stack
import GHC.TypeNats (KnownNat, type (+), type (*))
import System.Random (RandomGen, Random, random)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Data.List (findIndex)
import Data.Foldable (toList)
import Data.Monoid
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap, fromList, lookup)
import Data.Proxy
import System.Environment (getEnv)
import Control.Exception (SomeException, try, assert)
import Control.Monad (void, foldM, (=<<))
import Language.Haskell.Interpreter (Interpreter)
import Language.Haskell.Exts.Syntax

import           Torch.Typed.Aux
import           Torch.Typed.Tensor hiding (dim)
import qualified Torch.Typed.Tensor
import           Torch.Typed.Functional
import           Torch.Typed.Parameter
import qualified Torch.Typed.Parameter
import           Torch.Typed.NN
import           Torch.Typed.NN.Recurrent.LSTM
import           Torch.HList
import qualified Torch.NN                      as A
import           Torch.Autograd                as D
import           Torch.TensorFactories         as D
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
import qualified Torch.Device                  as D
import qualified Torch.Optim                   as D
import qualified Torch.Functional.Internal     as I
import qualified Torch.Functional              as F
import qualified Torch.Internal.Class                    as ATen
import qualified Torch.Internal.Managed.Native           as ATen
import qualified Torch.Internal.Unmanaged.Type.Context   as ATen

import Synthesis.Data (Expr, Tp)
import Synthesis.Utility (pp, fisherYates)
import Synthesis.Ast (genBlockVariants)
import Synthesis.Hint (exprType)

import System.IO.Unsafe (unsafePerformIO)
import Torch.Internal.Cast

type Dir = 'Bidirectional
type Dirs = NumberOfDirections Dir
dirs :: Int
dirs = natValI @Dirs

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

cpu = Proxy @'( 'D.CPU, 0)

cuda0 = Proxy @'( 'D.CUDA, 0)

availableDevices :: [D.Device]
availableDevices =
  let hasCuda = unsafePerformIO $ cast0 ATen.hasCUDA
  in  [D.Device { D.deviceType = D.CPU, D.deviceIndex = 0 }]
        <> (if hasCuda
             then [D.Device { D.deviceType = D.CUDA, D.deviceIndex = 0 }]
             else mempty
           )

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
        check = D.shape . toDynamic
        gold = optionsRuntimeShape @shape' @dtype' @device'
        in assertEqBy check gold tensor'

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

-- | point-free untyped select
select'' :: Int -> Int -> D.Tensor -> D.Tensor
select'' dim idx tensor = select' tensor dim idx

-- | spread the given dimension from a D.Tensor out as a list (keepdim=True)
unDim :: Int -> D.Tensor -> [D.Tensor]
unDim dim tensor = select' tensor dim <$> [0 .. (D.shape tensor !! dim) - 1]

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

-- | get holed variants for a DSL
dslVariants :: HashMap String Expr -> Interpreter [(String, Expr)]
dslVariants dsl = do
    fn_types :: HashMap String Tp <- exprType `mapM` dsl
    return $ genBlockVariants maxWildcardDepth fn_types
            where maxWildcardDepth = 0  -- no * here

-- | split a (batch-first) tensor into batches (zero-padded for the last one)
batchTensor :: Int -> D.Tensor -> [D.Tensor]
batchTensor batch_size tensor = let
    nDim :: Int = 0
    n :: Int = D.size tensor nDim
    numBatches :: Int = ((n-1) `div` batch_size) + 1
    diff :: Int = numBatches * batch_size - n
    paddings :: [Int] = replicate (2 * D.dim tensor - 1) 0 <> [diff]
    tensor' :: D.Tensor = F.constantPadNd1d paddings 0.0 tensor
    f :: Int -> D.Tensor = \i -> let
            from :: Int = i * batch_size
            to   :: Int = (i+1) * batch_size - 1
            idxs :: [Int] = [from .. to]
        in D.indexSelect tensor' nDim . D.asTensor $ asLong <$> idxs
    in f <$> [0 .. numBatches-1]

-- | statically typed version of batchTensor
-- | deprecated, not in use
batchTensor'
    :: forall batchSize dim device dtype shape shape'
     . ( KnownNat batchSize
       , KnownShape shape
       , TensorOptions shape dtype device
       , dim ~ 0
       , shape' ~ FromMaybe (ReplaceDim dim shape batchSize)
       )
    => Tensor device dtype shape
    -> [Tensor device dtype shape']
batchTensor' tensor = let
    batch_size = natValI @batchSize
    nDim = natValI @dim
    n :: Int = D.size (toDynamic tensor) nDim
    numBatches :: Int = ((n-1) `div` batch_size) + 1
    diff :: Int = numBatches * batch_size - n
    paddings :: [Int] = replicate (2 * Torch.Typed.Tensor.dim tensor - 1) 0 <> [diff]
    tensor' :: D.Tensor = F.constantPadNd1d paddings 0.0 $ toDynamic tensor
    f :: Int -> Tensor device dtype shape' = \i -> let
            from :: Int = i * batch_size
            to   :: Int = (i+1) * batch_size - 1
            idxs :: [Int] = [from .. to]
        in UnsafeMkTensor $ D.indexSelect tensor' nDim . D.asTensor $ asLong <$> idxs
    in f <$> [0 .. numBatches-1]

type family FromMaybe (maybe :: Maybe a) :: a where
  FromMaybe (Just a) = a
--   FromMaybe Nothing = Nothing

-- | deprecated, not in use
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

whenOr :: a -> Bool -> a -> a
-- whenOr def cond x = fromMaybe def $ toMaybe cond x
whenOr def cond x = if cond then x else def

whenOrM :: (Applicative m) => a -> Bool -> m a -> m a
whenOrM def cond x = if cond then x else pure def

-- | shuffle a tensor in a given dimension
-- | deprecated, not in use
shuffle :: forall g . (RandomGen g) => g -> Int -> D.Tensor -> (g, D.Tensor)
shuffle gen dim tensor = (gen', shuffled)
    where
        n = D.size tensor dim
        idxs = [0 .. n-1]
        (idxs', gen') = fisherYates gen idxs
        shuffled = D.indexSelect tensor dim $ D.asTensor $ asLong <$> idxs'

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
assertP :: (?loc :: CallStack, Show a) => (a -> Bool) -> a -> a
assertP pred_fn x = case pred_fn x of
    True -> x
    False -> error $ "assertP failed on input: " <> show x <> "\n" <> prettyCallStack ?loc

-- | assert an equality check by a mapper function
assertEqBy :: (?loc :: CallStack, Show b, Eq b) => (a -> b) -> b -> a -> a
assertEqBy fn gold x = let x' = fn x in case x' == gold of
    True -> x
    False -> error $ "equality check failed on input ( " <> show x' <> " ) with gold value ( " <> show gold <> " ):\n" <> prettyCallStack ?loc

-- | assert an equality check -- yields a nicer stack trace than assertP
assertEq :: (?loc :: CallStack, Show a, Eq a) => a -> a -> a
assertEq = assertEqBy id

-- | apply a softmax over all dimensions
softmaxAll :: D.Tensor -> D.Tensor
softmaxAll t = F.divScalar ((D.asValue $ F.sumAll e) :: Float) e
    where e = F.exp t

-- | loop n times, retaining state
foldLoop :: forall a b m . (Num a, Enum a, Monad m) => b -> a -> (b -> a -> m b) -> m b
foldLoop x count block = foldM block x ([1 .. count] :: [a])

-- | like np.unravel_idx, unravel a flat index (from e.g. argmax_t) to the dimensions of a tensor
unravelIdx :: D.Tensor -> Int -> [Int]
unravelIdx t idx = snd . foldr (\ dim_ (idx_, idxs) -> (idx_ `Prelude.div` dim_, idx_ `Prelude.mod` dim_ : idxs)) (idx, []) $ D.shape t

-- | create a reverse index (elements to indices) from a list
indexList :: (Eq a, Hashable a) => [a] -> HashMap a Int
indexList xs = fromList $ zip xs [0 .. length xs - 1]

-- TODO: replace with built-in
-- | calculate the cross-entropy loss given target indices, a class dimension, and a predictions tensor
crossEntropy :: D.Tensor -> Int -> D.Tensor -> D.Tensor
crossEntropy target dim input = F.nllLoss' target (F.logSoftmax dim input)

-- | TODO: replace with actual F.sumDim
f_sumDim :: Int -> D.Tensor -> D.Tensor
f_sumDim dim t = I.sumDim t dim False $ D.dtype t

-- -- | TODO: replace with actual F.squeezeDim
f_squeezeDim :: Int -> D.Tensor -> D.Tensor
f_squeezeDim dim t = I.squeezeDim t dim

-- TODO: import from hasktorch
f_multinomial_tlb
  :: D.Tensor
  -> Int
  -> Bool
  -> IO D.Tensor
f_multinomial_tlb t l b =
  (cast3 ATen.multinomial_tlb) t l b

-- | adjusted Torch.Typed.NN.Recurrent.LSTM.lstm to dynamically calculate batch size
-- | TODO: just batch inputs, ensuring dummy items won't influence results?
lstmDynamicBatch
  :: forall
       shapeOrder
       batchSize
       seqLen
       directionality
       initialization
       numLayers
       inputSize
       outputSize
       hiddenSize
       inputShape
       outputShape
       hxShape
       parameters
       tensorParameters
       dtype
       device
   . ( KnownNat (NumberOfDirections directionality)
     , KnownNat numLayers
     -- , KnownNat batchSize
     , KnownNat hiddenSize
     , KnownRNNShapeOrder shapeOrder
     , KnownRNNDirectionality directionality
     , outputSize ~ (hiddenSize * NumberOfDirections directionality)
     , inputShape ~ RNNShape shapeOrder seqLen batchSize inputSize
     , outputShape ~ RNNShape shapeOrder seqLen batchSize outputSize
     , hxShape ~ '[numLayers * NumberOfDirections directionality, batchSize, hiddenSize]
     , Parameterized (LSTM inputSize hiddenSize numLayers directionality dtype device) parameters
     , tensorParameters ~ LSTMR inputSize hiddenSize numLayers directionality dtype device
     , ATen.Castable (HList tensorParameters) [D.ATenTensor]
     , HMap' ToDependent parameters tensorParameters
     )
  => Bool
  -> LSTMWithInit
       inputSize
       hiddenSize
       numLayers
       directionality
       initialization
       dtype
       device
  -> Tensor device dtype inputShape
  -> ( Tensor device dtype outputShape
     , Tensor device dtype hxShape
     , Tensor device dtype hxShape
     )
lstmDynamicBatch dropoutOn (LSTMWithConstInit lstm@(LSTM _ (Dropout dropoutProb)) cc hc) input
  = Torch.Typed.Functional.lstm
    @shapeOrder
    @directionality
    @numLayers
    @seqLen
    @batchSize
    @inputSize
    @outputSize
    @hiddenSize
    @inputShape
    @outputShape
    @hxShape
    @tensorParameters
    @dtype
    @device
    (hmap' ToDependent . flattenParameters $ lstm)
    dropoutProb
    dropoutOn
    (cc', hc')
    input
 where
  cc' =
    asUntyped (
      D.reshape [natValI @(numLayers * NumberOfDirections directionality), batchSize, natValI @hiddenSize]
      . (\t -> F.expand t False [batchSize, natValI @(numLayers * NumberOfDirections directionality), natValI @hiddenSize])
      )
      $ cc
  hc' =
    asUntyped (
      D.reshape [natValI @(numLayers * NumberOfDirections directionality), batchSize, natValI @hiddenSize]
      . (\t -> F.expand t False [batchSize, natValI @(numLayers * NumberOfDirections directionality), natValI @hiddenSize])
      )
      $ hc
  batchSize = D.size (toDynamic input) $ if rnnBatchFirst @shapeOrder then 0 else 1

d_mkAdam
  :: Int
  -> Float
  -> Float
  -> [A.Parameter]
  -> D.Adam
d_mkAdam iter beta1 beta2 parameters =
    D.Adam
        beta1
        beta2
        (D.zerosLike . D.toDependent <$> parameters)
        (D.zerosLike . D.toDependent <$> parameters)
        iter

toBool :: forall device . Tensor device 'D.Bool '[] -> Bool
toBool t = D.asValue . toDynamic . toCPU $ t

untypeParam :: Parameter device dtype shape -> A.Parameter
untypeParam (UnsafeMkParameter param) = param

instance () => A.Parameterized (LSTMWithInit inputSize hiddenSize numLayers directionality initialization dtype device) where
  flattenParameters LSTMWithConstInit{..} =
           A.flattenParameters lstmWithConstInit_lstm
  flattenParameters LSTMWithLearnedInit{..} =
           A.flattenParameters lstmWithLearnedInit_lstm
        ++ fmap untypeParam [lstmWithLearnedInit_c, lstmWithLearnedInit_h]
  replaceOwnParameters LSTMWithConstInit{..} = do
    lstmWithConstInit_lstm' <- A.replaceOwnParameters lstmWithConstInit_lstm
    return $ LSTMWithConstInit
                 { lstmWithConstInit_lstm = lstmWithConstInit_lstm'
                 , ..
                 }
  replaceOwnParameters LSTMWithLearnedInit{..} = do
    lstmWithLearnedInit_lstm' <- A.replaceOwnParameters lstmWithLearnedInit_lstm
    lstmWithLearnedInit_c'    <- A.nextParameter
    lstmWithLearnedInit_h'    <- A.nextParameter
    return $ LSTMWithLearnedInit
                 { lstmWithLearnedInit_lstm = lstmWithLearnedInit_lstm'
                 , lstmWithLearnedInit_c    = UnsafeMkParameter lstmWithLearnedInit_c'
                 , lstmWithLearnedInit_h    = UnsafeMkParameter lstmWithLearnedInit_h'
                 }

instance () => A.Parameterized (LSTM inputSize hiddenSize numLayers directionality dtype device) where
  flattenParameters LSTM{..} = A.flattenParameters lstm_layer_stack
  replaceOwnParameters LSTM{..} = do
    lstm_layer_stack' <- A.replaceOwnParameters lstm_layer_stack
    return $ LSTM
                 { lstm_layer_stack = lstm_layer_stack'
                 , ..
                 }

instance () => A.Parameterized (LSTMLayerStack inputSize hiddenSize numLayers directionality dtype device) where
  flattenParameters (LSTMLayer1 layer) =
           A.flattenParameters layer
  flattenParameters (LSTMLayerK stack layer) =
           A.flattenParameters stack
        ++ A.flattenParameters layer
  replaceOwnParameters (LSTMLayer1 layer) = do
    layer' <- A.replaceOwnParameters layer
    return $ LSTMLayer1 layer'
  replaceOwnParameters (LSTMLayerK stack layer) = do
    stack' <- A.replaceOwnParameters stack
    layer' <- A.replaceOwnParameters layer
    return $ LSTMLayerK stack' layer'

instance () => A.Parameterized (LSTMLayer inputSize hiddenSize directionality dtype device) where
  flattenParameters (LSTMUnidirectionalLayer wi wh bi bh) =
      [ untypeParam wi
      , untypeParam wh
      , untypeParam bi
      , untypeParam bh
      ]
  flattenParameters (LSTMBidirectionalLayer wi wh bi bh wi' wh' bi' bh') =
      [ untypeParam wi
      , untypeParam wh
      , untypeParam bi
      , untypeParam bh
      , untypeParam wi'
      , untypeParam wh'
      , untypeParam bi'
      , untypeParam bh'
      ]
  replaceOwnParameters (LSTMUnidirectionalLayer _wi _wh _bi _bh) = do
    wi <- A.nextParameter
    wh <- A.nextParameter
    bi <- A.nextParameter
    bh <- A.nextParameter
    return (LSTMUnidirectionalLayer
            (UnsafeMkParameter wi)
            (UnsafeMkParameter wh)
            (UnsafeMkParameter bi)
            (UnsafeMkParameter bh)
            )
  replaceOwnParameters (LSTMBidirectionalLayer _wi _wh _bi _bh _wi' _wh' _bi' _bh') = do
    wi <- A.nextParameter
    wh <- A.nextParameter
    bi <- A.nextParameter
    bh <- A.nextParameter
    wi' <- A.nextParameter
    wh' <- A.nextParameter
    bi' <- A.nextParameter
    bh' <- A.nextParameter
    return (LSTMBidirectionalLayer
            (UnsafeMkParameter wi)
            (UnsafeMkParameter wh)
            (UnsafeMkParameter bi)
            (UnsafeMkParameter bh)
            (UnsafeMkParameter wi')
            (UnsafeMkParameter wh')
            (UnsafeMkParameter bi')
            (UnsafeMkParameter bh')
            )

instance (Foldable t, Traversable t, A.Parameterized a) => A.Parameterized (t a) where
  flattenParameters = (=<<) A.flattenParameters . toList
  replaceOwnParameters = mapM A.replaceOwnParameters

instance A.Parameterized (Parameter device dtype shape) where
  flattenParameters (UnsafeMkParameter param) = pure param
  replaceOwnParameters _ = UnsafeMkParameter <$> A.nextParameter
