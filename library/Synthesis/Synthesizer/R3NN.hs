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
{-# LANGUAGE UndecidableInstances #-}
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
import Torch.Autograd
import Torch.Typed.NN
import Torch.Typed.NN.Recurrent.LSTM
import Torch.HList
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Tensor                  as D
import qualified Torch.Device                  as D
import qualified Torch.DType                   as D
import qualified Torch.TensorFactories         as D
import qualified Torch.Functional.Internal     as I
import qualified Torch.Autograd                as D

import Synthesis.Orphanage ()
import Synthesis.Data (Expr, Tp)
import Synthesis.Types (holeType)
import Synthesis.FindHoles
import Synthesis.Synthesizer.Utility
import Synthesis.Synthesizer.UntypedMLP
import Synthesis.Synthesizer.TypeEncoder
import Synthesis.Synthesizer.Params

data R3NNSpec
    (device :: (D.DeviceType, Nat))
    (m :: Nat)
    (symbols :: Nat)
    (rules :: Nat)
    (maxStringLength :: Nat)
    (batch_size :: Nat)
    (h :: Nat)
    (maxChar :: Nat)
    (featMult :: Nat)
 where
    R3NNSpec :: forall device m symbols rules maxStringLength batch_size h maxChar featMult
        -- symbolIdxs :: HashMap String Int
        -- ppt :: Expr
      . { variant_sizes :: HashMap String Int
        , conditionSpec :: LSTMSpec (m + batch_size * maxStringLength * (2 * featMult * Dirs * h)) (Div m Dirs) NumLayers Dir 'D.Float device
        , scoreSpec     :: LSTMSpec  m                                                             (Div m Dirs) NumLayers Dir 'D.Float device
        , holeEncoderSpec :: TypeEncoderSpec device maxStringLength maxChar m
        }
        -> R3NNSpec device m symbols rules maxStringLength batch_size h maxChar featMult
 deriving (Show)

data R3NN
    (device :: (D.DeviceType, Nat))
    (m          :: Nat)
    (symbols    :: Nat)
    (rules      :: Nat)
    (maxStringLength :: Nat)
    (batch_size :: Nat)
    (h          :: Nat)
    (maxChar    :: Nat)
    (featMult   :: Nat)
    -- I imagine NSPS fixed their batch to the sample size, but I have those for each type instantiation, making this harder for me to fix. as a work-around, I'm sampling instead.
 where
    R3NN :: forall m symbols rules maxStringLength batch_size device h maxChar featMult
      . { condition_model :: LSTMWithInit (m + batch_size * maxStringLength * (2 * featMult * Dirs * h)) (Div m Dirs) NumLayers Dir 'ConstantInitialization 'D.Float device
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
        , holeEncoder :: TypeEncoder device maxStringLength maxChar m
        }
        -> R3NN device m symbols rules maxStringLength batch_size h maxChar featMult
 deriving (Show, Generic)

-- cannot use static Parameterized, as the contents of left_nnets / right_nnets are not statically known in its current HashMap type
instance ( KnownNat m
         , KnownNat symbols
         , KnownNat rules
         , KnownNat maxStringLength
         , KnownNat batch_size
         , KnownNat h
         , KnownNat maxChar
         )
  => A.Parameterized (R3NN device m symbols rules maxStringLength batch_size h maxChar featMult)

instance ( KnownDevice device
         , RandDTypeIsValid device 'D.Float
         , KnownNat m
         , KnownNat symbols
         , KnownNat rules
         , KnownNat maxStringLength
         , KnownNat batch_size
         , KnownNat h
         , KnownNat maxChar
         , KnownNat featMult
         )
  => A.Randomizable (R3NNSpec device m symbols rules maxStringLength batch_size h maxChar featMult)
                    (R3NN     device m symbols rules maxStringLength batch_size h maxChar featMult)
 where
    sample R3NNSpec {..} = do
        join . return $ R3NN
            -- condition_model
            <$> A.sample (LSTMWithZerosInitSpec conditionSpec)
            -- score_model
            <*> A.sample (LSTMWithZerosInitSpec scoreSpec)
            -- left: untyped as q is not static
            <*> mapM (\q -> A.sample $ MLPSpec (q * m) m) variant_sizes
            -- right: ditto
            <*> mapM (\q -> A.sample $ MLPSpec m (q * m)) variant_sizes
            -- symbol_emb
            <*> (fmap UnsafeMkParameter . D.makeIndependent =<< D.randnIO' [symbols, m])
            -- rule_emb
            <*> (fmap UnsafeMkParameter . D.makeIndependent =<< D.randnIO' [rules,   m])
            -- hole encoder
            <*> A.sample holeEncoderSpec
            where
                -- m must be divisible by Dirs for `Div` in the LSTM specs to work out due to integer division...
                m = assertP ((== 0) . (`mod` natValI @Dirs)) $ natValI @m
                symbols = natValI @symbols
                rules = natValI @rules

-- instance ( KnownNat symbols, KnownNat m, KnownNat maxStringLength, KnownNat h, KnownNat rules, KnownNat batch_size, KnownDevice device, MatMulDTypeIsValid device 'D.Float, shape ~ '[batch_size, maxStringLength * (2 * featMult * Dirs * h)] )
--     => HasForward (R3NN device m symbols rules maxStringLength batch_size h maxChar featMult) (Tensor device 'D.Float shape) (Tensor device 'D.Float '[num_holes, rules]) where
--         forward      = runR3nn
--         -- forwardStoch = runR3nn

-- | initialize R3NN spec
initR3nn :: forall m symbols rules maxStringLength batch_size h device maxChar featMult
         . (KnownNat m, KnownNat symbols, KnownNat rules, KnownNat maxStringLength, KnownNat batch_size, KnownNat h, KnownNat featMult)
         => [(String, Expr)]
         -> Int
         -> Double
         -> Int
         -> Int
         -> HashMap Char Int
         -> (R3NNSpec device m symbols rules maxStringLength batch_size h maxChar featMult)
initR3nn variants batch_size dropoutRate charMap = R3NNSpec @device @m @symbols @rules @maxStringLength @batch_size @h @maxChar @featMult
        variant_sizes
        -- condition
        (LSTMSpec $ DropoutSpec dropoutRate)
        -- score
        (LSTMSpec $ DropoutSpec dropoutRate)
        -- hole encoder
        (TypeEncoderSpec charMap $ LSTMSpec $ DropoutSpec dropoutRate)
    where
        maxStringLength :: Int = natValI @maxStringLength
        m :: Int = natValI @m
        h :: Int = natValI @h
        featMult :: Int = natValI @featMult 
        -- TODO: can I really cram all that back into just M?
        conditionIn = m + batch_size * 2 * featMult * dirs * h * maxStringLength
        variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants

-- | Recursive Reverse-Recursive Neural Network (R3NN) (Parisotto et al.)
runR3nn
    :: forall symbols m maxStringLength h rules batch_size num_holes device maxChar featMult
    . ( KnownNat symbols, KnownNat m, KnownNat maxStringLength, KnownNat h, KnownNat rules, KnownNat batch_size, KnownNat maxChar, KnownNat featMult, KnownDevice device, MatMulDTypeIsValid device 'D.Float )
    => R3NN device m symbols rules maxStringLength batch_size h maxChar featMult
    -> HashMap String Int
    -> Expr
    -> Tensor device 'D.Float '[rules, maxStringLength * m]
    -> Tensor device 'D.Float '[batch_size, maxStringLength * (2 * featMult * Dirs * h)]
    -- -> IO (Tensor device 'D.Float '[num_holes, rules])
    -> Tensor device 'D.Float '[num_holes, rules]
-- runR3nn r3nn symbolIdxs ppt io_feats = do
runR3nn r3nn symbolIdxs ppt rule_tp_emb io_feats = scores where
    R3NN{..} = r3nn
    device = deviceVal @device
    -- | Pre-conditioning: example encodings are concatenated to the encoding of each tree leaf
    io_feats' :: Tensor device 'D.Float '[batch_size * (maxStringLength * (2 * featMult * Dirs * h))] =
            -- reshape io_feats
            asUntyped (D.reshape [natValI @batch_size * (natValI @maxStringLength * (2 * natValI @featMult * natValI @Dirs * natValI @h))]) io_feats
    conditioned :: Tensor device 'D.Float '[symbols, m + batch_size * maxStringLength * (2 * featMult * Dirs * h)] =
            UnsafeMkTensor $ F.cat (F.Dim 1) [(D.toDevice device . toDynamic . Torch.Typed.Parameter.toDependent $ symbol_emb), stacked_io]
            where stacked_io = repeatDim 0 (natValI @symbols) $ toDynamic io_feats'
    -- conditioning can use an MLP or (bidir) LSTM; LSTM learns more about the relative position of each leaf node in the tree.
    conditioned' :: Tensor device 'D.Float '[symbols, m] = 
            -- asUntyped to type-check m*2/2
            asUntyped (\t -> I.squeezeDim t 0) .
            fstOf3 . lstmWithDropout @'SequenceFirst condition_model . unsqueeze @0 $ conditioned
    root_emb :: Tensor device 'D.Float '[1, m] = forwardPass @m r3nn symbolIdxs conditioned' ppt
    node_embs :: Tensor device 'D.Float '[num_holes, m] = 
            UnsafeMkTensor $ F.cat (F.Dim 0) $ toDynamic <$> reversePass @m r3nn root_emb ppt
    -- | bidirectional LSTM to process the global leaf representations right before calculating the scores.
    node_embs' :: Tensor device 'D.Float '[num_holes, m] =
            -- asUntyped to type-check m*2/2
            asUntyped (\t -> I.squeezeDim t 0) .
            fstOf3 . lstmDynamicBatch @'SequenceFirst dropoutOn score_model . unsqueeze @0 $ node_embs
                    where dropoutOn = True
    getters = fst <$> findHolesExpr ppt
    -- TODO: propagate constraints through to the hole types
    holeTypes :: [Tp] = holeType . (\gtr -> gtr ppt) <$> getters
    holeTypeEmb :: Tensor device 'D.Float '[num_holes, maxStringLength * m] =
        typeEncoder @num_holes @maxStringLength @maxChar @device @m holeEncoder holeTypes
    -- | expansion score z_e=ϕ′(e.l)⋅ω(e.r), for expansion e, expansion type e.r (for rule r∈R), leaf node e.l
    useTypes = natValI @featMult > 1
    scores :: Tensor device 'D.Float '[num_holes, rules] =
        if useTypes then matmul (cat @1
                (  node_embs'
                :. holeTypeEmb
                :. HNil
                ))
            . Torch.Typed.Tensor.toDevice . transpose @0 @1 $
            cat @1
                ( (Torch.Typed.Tensor.toDevice @device . Torch.Typed.Parameter.toDependent $ rule_emb)
                :. rule_tp_emb
                :. HNil
                )
        else matmul node_embs' . Torch.Typed.Tensor.toDevice . transpose @0 @1 $ Torch.Typed.Parameter.toDependent rule_emb
    -- delay softmax for log-sum-exp trick (crossEntropy)

variantInt :: Expr -> (String, Int)
variantInt = (appRule &&& length) . fnAppNodes

-- | Torch gets sad not all nnets get used in the loss 😢 so let's give it a hug... 🤗🙄
patchR3nnLoss :: forall m symbols rules maxStringLength batch_size device h maxChar featMult . (KnownNat m, KnownDevice device, SumDTypeIsValid device 'D.Float, BasicArithmeticDTypeIsValid device 'D.Float) => R3NN device m symbols rules maxStringLength batch_size h maxChar featMult -> HashMap String Int -> Tensor device 'D.Float '[] -> Tensor device 'D.Float '[]
patchR3nnLoss r3nn_model variant_sizes = let
        m :: Int = natValI @m
        left_dummy  :: Tensor device 'D.Float '[] = mulScalar (0.0 :: Float) $ sumAll $ Torch.Typed.Tensor.toDType @'D.Float . UnsafeMkTensor $ F.cat (F.Dim 1) $ fmap (\(k,mlp_) -> let q = variant_sizes ! k in mlp mlp_ $ D.zeros' [1,q*m]) $ toList $  left_nnets r3nn_model
        right_dummy :: Tensor device 'D.Float '[] = mulScalar (0.0 :: Float) $ sumAll $ Torch.Typed.Tensor.toDType @'D.Float . UnsafeMkTensor $ F.cat (F.Dim 1) $ fmap (\   mlp_  ->                              mlp mlp_ $ D.zeros' [1,  m]) $ elems  $ right_nnets r3nn_model
    in add $ Torch.Typed.Tensor.toDevice $ left_dummy `add` right_dummy

-- | perform a recursive pass going up in the tree to assign a global tree representation to the root.
forwardPass
    :: forall m symbols maxStringLength rules batch_size device h maxChar featMult
     . R3NN device m symbols rules maxStringLength batch_size h maxChar featMult
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
                nnet = asCPU . mlp . lookupRule left_nnets $ nodeRule expr
            in UnsafeMkTensor $ nnet $ F.cat (F.Dim 1) $ toDynamic <$> tensors
        _ -> error $ "unexpected Expr: " ++ show expr

-- | perform a reverse-recursive pass starting from the root to assign a global tree representation to each node in the tree.
-- | This produces a representation ϕ′(c) for each RHS node c of R(root).
-- note: order here *must* follow `findHolesExpr`!
reversePass
    :: forall m symbols maxStringLength rules batch_size device h maxChar featMult
    . ( KnownNat m )
    => R3NN device m symbols rules maxStringLength batch_size h maxChar featMult
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
                nnet = asCPU . mlp . lookupRule right_nnets $ nodeRule expr
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
