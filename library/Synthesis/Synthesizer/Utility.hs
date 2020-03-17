{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Synthesis.Synthesizer.Utility (
    Dev,
    getDevice,
    asUntyped,
    -- availableDevices,
    Dir,
    Dirs,
    Symbols,
    M,
    padRight,
    unDim,
    select',
    rotate,
    -- rotateT,
    fnAppNodes,
    nodeRule,
    appRule,
    lookupRule,
    variantSizes,
) where

import Prelude hiding (lookup)
import Data.Int (Int64)
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap, fromList, (!), size, keys, lookup)
import System.Environment (getEnv)
import Control.Exception (SomeException, try)
import GHC.TypeNats (Nat, KnownNat)
import Language.Haskell.Interpreter (Interpreter, lift, liftIO)
import Language.Haskell.Exts.Syntax

import Torch.Typed.Tensor
import Torch.Typed.Functional
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
import qualified Torch.Device                  as D

import Synthesis.Data (Expr, Tp, L)
import Synthesis.Utility (pp, pp_)
import Synthesis.Ast (genBlockVariants)
import Synthesis.Hint (exprType)

-- import qualified Torch.Internal.Managed.Type.Context     as ATen
-- import Torch.Internal.Cast (cast0)
-- import System.IO.Unsafe (unsafePerformIO)

type Dir = 'Bidirectional
type Dirs = NumberOfDirections Dir
-- hm, I'm not sure if NSPS counted hole as a symbol, as holes *have* symbols e.g. for me Expression, in which case there'd be nothing left to distinguish for me...
-- data Symbol = Variable | Hole
-- type Symbols = 2
type Symbols = 1 -- 2  -- holes also just get symbol Expression, so nothing left...
type M = 20 -- number of features for R3NN expansions/symbols. must be an even number for H.
type Dev = '( 'D.CPU, 0)

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

padRight :: a -> Int -> [a] -> [a]
padRight c n xs = xs ++ replicate (n - length xs) c

asUntyped :: forall device dtype shape shape'
           . (D.Tensor -> D.Tensor)
          -> Tensor device dtype shape
          -> Tensor device dtype shape'
asUntyped f = UnsafeMkTensor . f . toDynamic

-- | `select` alternative that retains the dimension as a 1
-- | I want this as a built-in, see https://github.com/pytorch/pytorch/issues/34788
select' :: D.Tensor -> Int -> Int -> D.Tensor
select' tensor dim idx = D.indexSelect tensor dim $ D.asTensor [(fromIntegral :: Int -> Int64) idx]

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
            fn : args -> unwords $ pp fn : replicate (length args) "_"

lookupRule :: (Eq k, Hashable k, Show k) => HashMap k v -> k -> v
lookupRule hm k = case (lookup k hm) of
    Just x -> x
    Nothing -> error $ "the DSL does not contain rule " ++ show k ++ "!"

variantSizes :: HashMap String Expr -> Interpreter ([(String, Expr)], HashMap String Int)
variantSizes dsl = do
    fn_types :: HashMap String Tp <- exprType `mapM` dsl
    -- liftIO . print $ "fn_types: " ++ pp_ fn_types
    let variants :: [(String, Expr)] = genBlockVariants maxWildcardDepth fn_types
            where maxWildcardDepth = 0  -- no * here
    -- liftIO . print $ "variants: " ++ pp_ variants
    let variant_sizes :: HashMap String Int = fromList $ variantInt `fmap` variants
            where variantInt :: (String, Expr) -> (String, Int) = \(k, expr) -> let
                        exprs = fnAppNodes expr
                        str :: String = appRule exprs
                        i :: Int = length exprs -- - 1 -- num of args
                    in (str, i)
    -- liftIO . print $ "variant_sizes: " ++ show variant_sizes
    return (variants, variant_sizes)
