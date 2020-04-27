{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Synthesis.Synthesizer.R3NN (module Synthesis.Synthesizer.R3NN) where

import Data.HashMap.Lazy (HashMap, fromList, toList, (!), elems)
import Control.Arrow ((&&&))
import Control.Monad ((=<<), join)
import Language.Haskell.Exts.Syntax
import GHC.Generics
import GHC.TypeNats (KnownNat, Nat, Div, type (*), type (+))
import Util (fstOf3)

import Torch.Typed.Tensor
import Torch.Typed.Functional
import Torch.Typed.NN
import Torch.Typed.Aux
import Torch.Typed.Parameter
import qualified Torch.Typed.Parameter
import Torch.Typed.Factories
import Torch.TensorFactories (randnIO', zeros')
import Torch.Autograd
import Synthesis.Synthesizer.LSTM
import Torch.HList
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Tensor                  as D
import qualified Torch.Device                  as D
import qualified Torch.DType                   as D
import qualified Torch.Functional.Internal     as I
import qualified Torch.Autograd                as D

import Synthesis.Orphanage ()
import Synthesis.Data (Expr)
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.UntypedMLP
import Synthesis.Synthesizer.Params

data R3NNSpec
    (device :: (D.DeviceType, Nat))
    (m :: Nat)
    (symbols :: Nat)
    (rules :: Nat)
    (t :: Nat)
    (batch_size :: Nat)
 where
    R3NNSpec :: forall device m symbols rules t batch_size
      . { variant_sizes :: HashMap String Int
        , conditionSpec :: LSTMSpec (m + batch_size * t * (2 * Dirs * H)) (Div m Dirs) NumLayers Dir 'D.Float device
        , scoreSpec     :: LSTMSpec  m                                        (Div m Dirs) NumLayers Dir 'D.Float device
        , leftH0  :: Int
        , leftH1  :: Int
        , rightH0 :: Int
        , rightH1 :: Int
        }
        -> R3NNSpec device m symbols rules t batch_size
 deriving (Show)

data R3NN
    (device :: (D.DeviceType, Nat))
    (m          :: Nat)
    (symbols    :: Nat)
    (rules      :: Nat)
    (t          :: Nat)
    (batch_size :: Nat)
    -- I imagine NSPS fixed their batch to the sample size, but I have those for each type instantiation, making this harder for me to fix. as a work-around, I'm sampling instead.
 where
    R3NN :: forall m symbols rules t batch_size device
      . { condition_model :: LSTMWithInit (m + batch_size * t * (2 * Dirs * H)) (Div m Dirs) NumLayers Dir 'ConstantInitialization 'D.Float device
        , score_model     :: LSTMWithInit  m                                    (Div m Dirs) NumLayers Dir 'ConstantInitialization 'D.Float device
        -- NSPS: for each production rule r∈R, a nnet f_r from x∈R^(Q⋅M) to y∈R^M,
        -- with Q as the number of symbols on the RHS of the production rule r.
        , left_nnets :: HashMap String MLP
        -- for each production rule r∈R, a nnet g_r from x'∈R^M to y'∈R^(Q⋅M).
        , right_nnets :: HashMap String MLP
        -- for each symbol s∈S, an M-dimensional representation ϕ(s)∈R^M.
        , symbol_emb :: Parameter device 'D.Float '[symbols, m]
        -- for each production rule r∈R, an M−dimensional representation: ω(r)∈R^M.
        , rule_emb   :: Parameter device 'D.Float '[rules  , m]
        }
        -> R3NN device m symbols rules t batch_size
 deriving (Show, Generic)

-- cannot use static Parameterized, as the contents of left_nnets / right_nnets are not statically known in its current HashMap type
instance ( KnownNat m
         , KnownNat symbols
         , KnownNat rules
         , KnownNat t
         , KnownNat batch_size
         )
  => A.Parameterized (R3NN device m symbols rules t batch_size)

instance ( KnownDevice device
         , KnownNat m
         , KnownNat symbols
         , KnownNat rules
         , KnownNat t
         , KnownNat batch_size
         )
  => A.Randomizable (R3NNSpec device m symbols rules t batch_size)
                    (R3NN     device m symbols rules t batch_size)
 where
    sample R3NNSpec {..} = do
        join . return $ R3NN
            -- condition_model
            <$> A.sample (LSTMWithZerosInitSpec conditionSpec)
            -- score_model
            <*> A.sample (LSTMWithZerosInitSpec scoreSpec)
            -- left: untyped as q is not static
            <*> mapM (\q -> A.sample $ MLPSpec (q * m) leftH0 leftH1 m) variant_sizes
            -- right: ditto
            <*> mapM (\q -> A.sample $ MLPSpec m rightH0 rightH1 (q * m)) variant_sizes
            -- symbol_emb
            <*> (fmap UnsafeMkParameter . D.makeIndependent =<< randnIO' [symbols, m])
            -- rule_emb
            <*> (fmap UnsafeMkParameter . D.makeIndependent =<< randnIO' [rules,   m])
            where
                -- m must be divisible by Dirs for `Div` in the LSTM specs to work out due to integer division...
                m = assertP ((== 0) . (`mod` natValI @Dirs)) $ natValI @m
                symbols = natValI @symbols
                rules = natValI @rules

-- | initialize R3NN spec
initR3nn :: forall m symbols rules t batch_size device
         . (KnownNat m, KnownNat symbols, KnownNat rules, KnownNat t, KnownNat batch_size)
         => [(String, Expr)]
         -> Int
         -> Double
         -> (R3NNSpec device m symbols rules t batch_size)
initR3nn variants batch_size dropoutRate = R3NNSpec @device @m @symbols @rules @t @batch_size
        variant_sizes
        -- condition
        (LSTMSpec $ DropoutSpec dropoutRate)
        -- score
        (LSTMSpec $ DropoutSpec dropoutRate)
        -- left
        hidden0
        hidden1
        -- right
        hidden0
        hidden1
    where
        t :: Int = natValI @t
        m :: Int = natValI @m
        -- TODO: can I really cram all that back into just M?
        conditionIn = m + batch_size * 2 * dirs * h * t
        variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants

-- | Recursive Reverse-Recursive Neural Network (R3NN) (Parisotto et al.)
runR3nn
    :: forall symbols m t rules batch_size num_holes device
    . ( KnownNat symbols, KnownNat m, KnownNat t, KnownNat batch_size )
    => R3NN device m symbols rules t batch_size
    -> HashMap String Int
    -> Expr
    -> Tensor device 'D.Float '[batch_size, t * (2 * Dirs * H)]
    -> IO (Tensor device 'D.Float '[num_holes, rules])
runR3nn r3nn symbolIdxs ppt io_feats = do
    let R3NN{..} = r3nn
    -- | Pre-conditioning: example encodings are concatenated to the encoding of each tree leaf
    let io_feats' :: Tensor device 'D.Float '[batch_size * (t * (2 * Dirs * H))] = reshape io_feats
    let conditioned :: Tensor device 'D.Float '[symbols, m + batch_size * t * (2 * Dirs * H)] =
            UnsafeMkTensor $ F.cat (F.Dim 1) [toDynamic (Torch.Typed.Parameter.toDependent symbol_emb), stacked_io]
            where stacked_io = stack' 0 $ replicate (natValI @symbols) $ toDynamic io_feats'
    -- conditioning can use an MLP or (bidir) LSTM; LSTM learns more about the relative position of each leaf node in the tree.
    let conditioned' :: Tensor device 'D.Float '[symbols, m] = 
            -- asUntyped to type-check m*2/2
            asUntyped (\t -> I.squeezeDim t 0) .
            fstOf3 . lstmWithDropout @'SequenceFirst condition_model . unsqueeze @0 $ conditioned
    let root_emb :: Tensor device 'D.Float '[1, m] = forwardPass @m r3nn symbolIdxs conditioned' ppt
    let node_embs :: Tensor device 'D.Float '[num_holes, m] = 
            UnsafeMkTensor $ F.cat (F.Dim 0) $ toDynamic <$> reversePass @m r3nn root_emb ppt
    -- | bidirectional LSTM to process the global leaf representations right before calculating the scores.
    let node_embs' :: Tensor device 'D.Float '[num_holes, m] =
            -- asUntyped to type-check m*2/2
            asUntyped (\t -> I.squeezeDim t 0) .
            fstOf3 . lstmDynamicBatch @'SequenceFirst dropoutOn score_model . unsqueeze @0 $ node_embs
                    where dropoutOn = True
    -- | expansion score z_e=ϕ′(e.l)⋅ω(e.r), for expansion e, expansion type e.r (for rule r∈R), leaf node e.l
    let scores :: Tensor device 'D.Float '[num_holes, rules] =
            matmul node_embs' . transpose @0 @1 $ Torch.Typed.Parameter.toDependent rule_emb
    -- delay softmax for log-sum-exp trick (crossEntropy)
    return scores

variantInt :: Expr -> (String, Int)
variantInt = (appRule &&& length) . fnAppNodes

-- | Torch gets sad not all nnets get used in the loss 😢 so let's give it a hug... 🤗🙄
patchLoss :: forall m symbols rules t batch_size device . (KnownNat m) => HashMap String Int -> R3NN device m symbols rules t batch_size -> Tensor device 'D.Float '[] -> Tensor device 'D.Float '[]
patchLoss variant_sizes r3nn_model = let
        m :: Int = natValI @m
        left_dummy  :: Tensor device 'D.Float '[] = mulScalar (0.0 :: Float) $ sumAll (Torch.Typed.Tensor.toDType @'D.Float . UnsafeMkTensor $ F.cat (F.Dim 1) $ fmap (\(k,mlp_) -> let q = variant_sizes ! k in mlp mlp_ $ zeros' [1,q*m]) $ toList $  left_nnets r3nn_model)
        right_dummy :: Tensor device 'D.Float '[] = mulScalar (0.0 :: Float) $ sumAll (Torch.Typed.Tensor.toDType @'D.Float . UnsafeMkTensor $ F.cat (F.Dim 1) $ fmap (\   mlp_  ->                              mlp mlp_ $ zeros' [1,  m]) $ elems  $ right_nnets r3nn_model)
    in add $ left_dummy `add` right_dummy

-- | perform a recursive pass going up in the tree to assign a global tree representation to the root.
forwardPass
    :: forall m symbols t rules batch_size device
     . R3NN device m symbols rules t batch_size
    -> HashMap String Int
    -> Tensor device 'D.Float '[symbols, m]
    -> Expr
    -> Tensor device 'D.Float '[1, m]
forwardPass r3nn symbolIdxs conditioned' expr = let
        R3NN{..} = r3nn
        f = forwardPass @m r3nn symbolIdxs conditioned'
    in case expr of
        Paren _l xpr -> f xpr
        Let _l _binds xpr -> f xpr
        ExpTypeSig _l xpr _tp -> f xpr
        Var _l qname -> let idx = case qname of
                                UnQual _l name -> case name of
                                    Ident _l str -> symbolIdxs ! str
                                    _ -> error $ "unexpected Name: " ++ show name
                                _ -> error $ "unexpected QName: " ++ show qname
                            -- | for each leaf l∈L get its representation ϕ(S(l)).
                            in asUntyped (select'' 0 idx) conditioned'
        App _l _exp1 _exp2 -> let
                tensors :: [Tensor device 'D.Float '[1, m]] = f <$> fnAppNodes expr
                nnet = mlp $ lookupRule left_nnets $ nodeRule expr
            in UnsafeMkTensor $ nnet $ F.cat (F.Dim 1) $ toDynamic <$> tensors
        _ -> error $ "unexpected Expr: " ++ show expr

-- | perform a reverse-recursive pass starting from the root to assign a global tree representation to each node in the tree.
-- | This produces a representation ϕ′(c) for each RHS node c of R(root).
-- note: order here *must* follow `findHolesExpr`!
reversePass
    :: forall m symbols t rules batch_size device
    . ( KnownNat m )
    => R3NN device m symbols rules t batch_size
    -> Tensor device 'D.Float '[1, m]
    -> Expr
    -> [Tensor device 'D.Float '[1, m]]
reversePass r3nn node_emb expr = let
        R3NN{..} = r3nn
        f = reversePass @m r3nn node_emb
    in case expr of
        Paren _l xpr -> f xpr
        Let _l _binds xpr -> f xpr
        ExpTypeSig _l xpr _tp -> f xpr
        -- | If c is a leaf node, we now have a leaf representation ϕ′(c) which has an
        -- | information path to ϕ(root) and thus to every other leaf node in the tree.
        Var _l qname -> case qname of
            UnQual _l name -> case name of
                Ident _l str -> case str of
                    "undefined" -> [node_emb]  -- hole i.e. non-terminal, gotta keep this
                    _ -> []  -- terminal leaf, discard embedding
                _ -> error $ "unexpected Name: " ++ show name
            _ -> error $ "unexpected QName: " ++ show qname
        -- | If c is a non-leaf node, we iteratively apply this procedure to c
        App _l _exp1 _exp2 -> let
                nnet = mlp $ lookupRule right_nnets $ nodeRule expr
                -- Tensor device 'D.Float '[1, q * m]
                tensor :: D.Tensor = nnet $ toDynamic node_emb
                child_exprs :: [Expr] = fnAppNodes expr
                -- split tensor into q tensors of '[1, m]...
                q :: Int = length child_exprs
                tensors :: [Tensor device 'D.Float '[1, m]] =
                        UnsafeMkTensor <$> (unDim 0 . D.reshape [q, m] $ tensor)
                tensors' :: [Tensor device 'D.Float '[1, m]] =
                        uncurry (reversePass @m r3nn) =<< zip tensors child_exprs
            in tensors'
        _ -> error $ "unexpected Expr: " ++ show expr
    where m = natValI @m
