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

-- import Data.Word (Word64)
-- import Data.Bifunctor (second)
import Data.HashMap.Lazy (HashMap, fromList, (!)) -- , size, keys, lookup
import Control.Arrow ((&&&))
import Control.Monad ((=<<), join)
-- import Control.Monad.State.Strict (StateT(..))
import Language.Haskell.Exts.Syntax
import GHC.Generics -- (Generic)
import GHC.TypeNats (KnownNat, Nat, Div, type (*), type (+))  -- , Mod, type (-)
import Util (fstOf3)

import Torch.Typed.Tensor
import Torch.Typed.Functional
import Torch.Typed.NN
import Torch.Typed.Aux
import Torch.Typed.Parameter
import qualified Torch.Typed.Parameter
import Torch.Typed.Factories
import Torch.TensorFactories (randnIO')
import Torch.Autograd -- (IndependentTensor(..))
-- import Torch.TensorOptions
import Torch.Typed.NN.Recurrent.LSTM
import Torch.HList
-- import qualified Torch.HList
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
-- import qualified Torch.Device                  as D
import qualified Torch.Functional.Internal     as I
-- import qualified Torch.Random                  as D
import qualified Torch.Autograd                as D
-- import Torch.Random (Generator)

import Synthesis.Orphanage ()
import Synthesis.Data (Expr)  -- , Tp, L
-- import Synthesis.Types (appFn, appArg, unQName)
-- import Synthesis.Utility (pp, pp_)
-- import Synthesis.Blocks (blockAsts)
-- import Synthesis.FindHoles (findHolesExpr, findIdentExpr, findFnAppExpr, findHolesExpr', findIdentExpr', findFnAppExpr', findTopFnAppExpr')
import Synthesis.Synthesizer.Utility
import qualified Synthesis.Synthesizer.Encoder as Enc
import qualified Synthesis.Synthesizer.UntypedMLP as UntypedMLP
-- import qualified Synthesis.Synthesizer.TypedMLP as TypedMLP

-- now shared between left/right nns, no clue how many layers or even hidden units I'd want
-- pre-score lstm
-- type NumLayers = 3 -- ?
-- | H is the topmost LSTM hidden dimension
-- type H = Div M Dirs -- needed for node_embs' with bidir lstm to still output a size that can be dot-product'd with rule_emb
-- dropoutRate :: Double
-- dropoutRate = 0.0 -- drop-out not mentioned in NSPS

type NumLayers = 3 -- ?

-- | H is the topmost LSTM hidden dimension
-- type H = 30 -- ?
-- h :: Int
-- h = natValI @H

data R3NNSpec
    (m :: Nat)
    -- (m :: KnownNat)
    (symbols :: Nat)
    (rules :: Nat)
    (t :: Nat)
    (batch_size :: Nat)
 where
  R3NNSpec :: forall m symbols rules t batch_size
     . {
        -- gen :: Generator,
        variant_sizes :: HashMap String Int,    -- dsl
        -- conditionSpec :: UntypedMLP.MLPSpec,
        conditionSpec :: LSTMSpec (m + batch_size * t * (2 * Dirs * Enc.H)) (Div m Dirs) NumLayers Dir 'D.Float Dev,
        -- scoreSpec :: LSTMSpec m m NumLayers Dir 'D.Float Dev,
        scoreSpec     :: LSTMSpec  m                                        (Div m Dirs) NumLayers Dir 'D.Float Dev,
        leftH0  :: Int,
        leftH1  :: Int,
        rightH0 :: Int,
        rightH1 :: Int
        }
    -> R3NNSpec m symbols rules t batch_size
 deriving (Show)

data R3NN
    (m          :: Nat)
    (symbols    :: Nat)
    (rules      :: Nat)
    (t          :: Nat)
    (batch_size :: Nat)
 where
  R3NN
    :: forall m symbols rules t batch_size
    -- htan activation
    --  . { condition_model :: UntypedMLP.MLP
     . { condition_model :: LSTMWithInit (m + batch_size * t * (2 * Dirs * Enc.H)) (Div m Dirs) NumLayers Dir 'ConstantInitialization 'D.Float Dev
       , score_model     :: LSTMWithInit  m                                        (Div m Dirs) NumLayers Dir 'ConstantInitialization 'D.Float Dev
    --    , score_model :: LSTMWithInit m m NumLayers Dir 'ConstantInitialization 'D.Float Dev
        -- For every production rule r∈R, a deep neural network f_r which takes as
        -- input a vector x∈R^Q⋅M, with Q being the number of symbols on the RHS of
        -- the production rule r, and outputs a vector y∈R^M.
        -- Therefore, the production-rule network f_r takes as input a concatenation
        -- of the distributed representations of each of its RHS symbols and produces
        -- a distributed representation for the LHS symbol.
       , left_nnets :: HashMap String UntypedMLP.MLP
        -- For every production rule r∈R, an additional deep neural network g_r which takes as input a vector x′∈R^M and outputs a vector y′∈R^Q⋅M.
        -- We can think of g_r as a reverse production-rule network that takes as input a vector representation of the LHS and produces a concatenation of the distributed representations of each of the rule’s RHS symbols.
       , right_nnets :: HashMap String UntypedMLP.MLP
        -- The R3NN has the following parameters for the grammar described by a DSL (see Figure 3):
        -- For every symbol s∈S, an M-dimensional representation ϕ(s)∈R^M.
       , symbol_emb :: Parameter Dev 'D.Float '[symbols, m]  -- NSPS's phi   -- A.Parameter
    --    , symbol_emb            :: A.Parameter
        -- For every production rule r∈R, an M−dimensional representation: ω(r)∈R^M.
       , rule_emb   :: Parameter Dev 'D.Float '[rules  , m]  -- NSPS's omega -- A.Parameter
    --    , rule_emb :: A.Parameter
        }
    -> R3NN m symbols rules t batch_size
 deriving (Show, Generic)

-- cannot use static Parameterized, as the contents of left_nnets / right_nnets are not statically known in its current HashMap type
instance ( KnownNat m
         , KnownNat symbols
         , KnownNat rules
         , KnownNat t
         , KnownNat batch_size
         )
--   => A.Parameterized (R3NN m symbols rules t batch_size)
  => A.Parameterized (R3NN m symbols rules t batch_size) where
  flattenParameters R3NN{..} =
        A.flattenParameters condition_model
        ++ A.flattenParameters score_model
        ++ A.flattenParameters left_nnets
        ++ A.flattenParameters right_nnets
        ++ [ untypeParam symbol_emb
            , untypeParam   rule_emb
            ]
  replaceOwnParameters R3NN{..} = do
        condition_model' <- A.replaceOwnParameters condition_model
        score_model'     <- A.replaceOwnParameters     score_model
        left_nnets'      <- A.replaceOwnParameters  left_nnets
        right_nnets'     <- A.replaceOwnParameters right_nnets
        symbol_emb' <- A.nextParameter
        rule_emb'   <- A.nextParameter
        return $ R3NN{ condition_model = condition_model'
                    ,     score_model =     score_model'
                    ,  left_nnets =  left_nnets'
                    , right_nnets = right_nnets'
                    , symbol_emb = UnsafeMkParameter symbol_emb'
                    ,   rule_emb = UnsafeMkParameter   rule_emb'
                    }

instance ( KnownNat m
         , KnownNat symbols
         , KnownNat rules
         , KnownNat t
         , KnownNat batch_size
         )
  => A.Randomizable (R3NNSpec m symbols rules t batch_size)
                    (R3NN     m symbols rules t batch_size)
 where
    sample R3NNSpec {..} = do
        join . return $ R3NN
            -- condition_model
            -- <$> A.sample conditionSpec
            <$> A.sample (LSTMWithZerosInitSpec conditionSpec)
            -- (LSTMWithZerosInitSpec . LSTMSpec . DropoutSpec $ dropoutRate)
            -- score_model
            <*> A.sample (LSTMWithZerosInitSpec scoreSpec)
            -- (TypedMLP.MLPSpec
            --         @(m + batch_size * t * (2 * Dirs * Enc.H)) @m
            --         @H0 @H1
            --         @D.Float
            --         D.Device
            --         dropoutRate
            --     )
            -- (LSTMWithZerosInitSpec . LSTMSpec . DropoutSpec $ dropoutRate)
            -- left
            -- untyped as q is not static
            <*> mapM (\q -> A.sample $ UntypedMLP.MLPSpec (q * m) leftH0 leftH1 m) variant_sizes
            -- right
            <*> mapM (\q -> A.sample $ UntypedMLP.MLPSpec m rightH0 rightH1 (q * m)) variant_sizes
            -- symbol_emb
            <*> (fmap UnsafeMkParameter . D.makeIndependent =<< randnIO' [symbols, m])
            -- <*> (return . IndependentTensor . toDynamic) symbol_emb
            -- rule_emb
            -- <*> (return . UnsafeMkParameter . IndependentTensor . toDynamic) rule_emb
            <*> (fmap UnsafeMkParameter . D.makeIndependent =<< randnIO' [rules,   m])
            -- <*> (return . IndependentTensor . toDynamic) rule_emb
            where
                -- m must be divisible by Dirs for `Div` in the LSTM specs to work out due to integer division...
                m = assertP ((== 0) . (`mod` natValI @Dirs)) $ natValI @m
                symbols = natValI @symbols
                rules = natValI @rules

-- | initialize R3NN spec
initR3nn :: forall m symbols rules t batch_size
         . (KnownNat m, KnownNat symbols, KnownNat rules, KnownNat t, KnownNat batch_size)
         => [(String, Expr)]
         -> Int
         -> Double
         -> (R3NNSpec m symbols rules t batch_size)
initR3nn variants batch_size dropoutRate = R3NNSpec @m @symbols @rules @t @batch_size
        variant_sizes
        -- condition
        -- (UntypedMLP.MLPSpec conditionIn h0 h1 m)
        (LSTMSpec $ DropoutSpec dropoutRate)
        -- score
        -- (UntypedMLP.MLPSpec           m h0 h1 m)
        (LSTMSpec $ DropoutSpec dropoutRate)
        -- left
        hiddenFeatures0
        hiddenFeatures1
        -- right
        hiddenFeatures0
        hiddenFeatures1
    where
        t :: Int = natValI @t
        m :: Int = natValI @m
        -- TODO: can I really cram all that back into just M?
        conditionIn = m + batch_size * 2 * dirs * Enc.h * t
        variant_sizes :: HashMap String Int = fromList $ variantInt . snd <$> variants
            where variantInt :: Expr -> (String, Int) = (appRule &&& length) . fnAppNodes

-- | Recursive Reverse-Recursive Neural Network (R3NN) (Parisotto et al.):
-- | given the continuous representation of the examples, synthesizes
-- | a program by incrementally expanding partial programs.
-- | Our generative model uses a Recursive-Reverse-Recursive Neural Network
-- | (R3NN) to encode partial trees (derivations) in L, where each node in the
-- | partial tree encodes global information about every other node in the tree.
-- | R3NN employs a tree-based neural architecture that sequentially
-- | constructs a parse tree by selecting which non-terminal symbol
-- | to expand using rules from a context-free grammar (i.e., the DSL).
-- | 
-- | 4.1 RECURSIVE-REVERSE-RECURSIVE NEURAL NETWORK
-- | 
-- | In order to define a generation model over PPTs, we need an efficient way
-- | of assigning probabilities to every valid expansion in the current PPT.
-- | A valid expansion has two components: first the production rule used, and
-- | second the position of the expanded leaf node relative to every other node
-- | in the tree.
-- | To account for the first component, a separate distributed representation
-- | for each production rule is maintained.
-- | The second component is handled using an architecture where the forward
-- | propagation resembles belief propagation on trees, allowing a notion of
-- | global tree state at every node within the tree.
-- | A given expansion probability is then calculated as being proportional to
-- | the inner product between the production rule representation and the
-- | global-tree representation of the leaf-level non-terminal node.
runR3nn
    :: forall symbols m t rules batch_size num_holes
    . ( KnownNat symbols, KnownNat m, KnownNat t, KnownNat batch_size )
    -- . ( KnownNat t )
    -- KnownNat m,
    -- KnownNat batch_size,
    -- KnownNat num_leaves,  -- Let L be the current leaf nodes of the PPT.
    -- KnownNat num_nodes,   -- Let N be the current non-leaf (rule) nodes of the PPT.??
    -- KnownNat num_holes,
    -- KnownNat rules)       -- Let E be the set of all valid expansions in a PPT.
    -- => R3NN m symbols rules
    => R3NN m symbols rules t batch_size
    -> HashMap String Int
    -> Expr
    -> Tnsr '[batch_size, t * (2 * Dirs * Enc.H)]
    -> IO (Tnsr '[num_holes, rules])
runR3nn
    R3NN{..}
    symbolIdxs
    ppt
    io_feats
    = do

    -- let t :: Int = natValI @t
    -- let batch_size :: Int = natValI @batch_size
    -- let n :: Int = shape' io_feats !! 0
    -- print $ "n: " ++ show n
    print $ "io_feats: " ++ show (shape' io_feats)
    -- let symbol_emb = toDynamic symbol_emb'

    -- | 5.2 CONDITIONING PROGRAM SEARCH ON EXAMPLE ENCODINGS
    -- | Once the I/O example encodings have been computed,
    -- | we can use them to perform conditional generation of the program tree using the R3NN model.
    -- | condition the PPT generation model using the I/O example encodings
    -- | Pre-conditioning: example encodings are concatenated to the encoding of each tree leaf,
    -- i/o features, identical for each leaf
    let io_feats' :: Tnsr '[batch_size * (t * (2 * Dirs * Enc.H))] = reshape io_feats
    print $ "io_feats': " ++ show (shape' io_feats')
    -- since these extra features don't depend on the leaf node, already concatenate them to `symbol_emb` instead of per leaf (`leaf_embs`) like in NSPS so for dim `symbols` instead of `NumLeaves`
    -- untyped as num_leaves is dynamic
    -- (toDynamic leaf_embs)
    -- let conditioned :: D.Tensor = F.cat 1 [leaf_embs, stack' 0 (replicate (shape' leaf_embs !! 0) (toDynamic io_feats'))]
    -- let conditioned :: D.Tensor = F.cat 1 [leaf_embs, stack' 0 (replicate (shape' leaf_embs !! 0) io_feats')]
    -- let stacked_io = stack 0 $ replicate symbols io_feats'
    -- print $ "stacked_io: " ++ show (shape' stacked_io)
    let conditioned :: Tnsr '[symbols, m + batch_size * t * (2 * Dirs * Enc.H)] =
            UnsafeMkTensor $ F.cat 1 [toDynamic (Torch.Typed.Parameter.toDependent symbol_emb), stacked_io]
            -- UnsafeMkTensor $ F.cat 1 [D.toDependent symbol_emb, stacked_io]
            where stacked_io = stack' 0 $ replicate (natValI @symbols) $ toDynamic io_feats' -- n
    print $ "conditioned: " ++ show (shape' conditioned)
    -- | and then passed to a conditioning network before the bottom-up recursive pass over the program tree.
    -- The conditioning network can be either a multi-layer feedforward network,
    -- -- let conditioned' :: Tnsr '[symbols, m] = TypedMLP.mlp condition_model train conditioned
    -- let conditioned' :: Tnsr '[symbols, m] =
    --         asUntyped (UntypedMLP.mlp condition_model) conditioned
    -- or a bidirectional LSTM network running over tree leaves.
    -- Running an LSTM over tree leaves allows the model to learn more about the relative position of each leaf node in the tree.
    let conditioned' :: Tnsr '[symbols, m] = 
            -- asUntyped id to type-check m*2/2
            asUntyped (\t -> I.squeezeDim t 0) .
            -- asUntyped id . 
            -- squeezeAll . -- unsafe, can squeeze out symbols too
            fstOf3 . lstmWithDropout @'SequenceFirst condition_model . unsqueeze @0 $ conditioned
    print $ "conditioned': " ++ show (shape' conditioned')

    -- | 4.1.1 Global Tree Information at the Leaves

    -- -- I'm now skipping this as I'm concatting i/o features straight to symbols while matching symbols is actually easier for me than matching leaves, given my Expr isn't uniquely identifiable (without proper SrcSpanInfo). this should simplify it will staying equivalent. I should test this later.
    -- -- Given a partial tree, the model first assigns a vector representation to each leaf node.
    -- -- | for every leaf node l∈L in the tree we retrieve its distributed representation ϕ(S(l)).
    -- let leaf_embs :: Tnsr '[NumLeaves, m] = UnsafeMkTensor $ F.cat 0 $ select' (toDynamic symbol_emb) 0 . symbolNum . leaf_symbol <$> leaves
    --                         where symbolNum = \case
    --                                 Hole -> 0
    --                                 Variable -> 1

    -- perform a recursive pass going up in the tree to assign a global tree representation to the root.
    -- | We now do a standard recursive bottom-to-top, RHS→LHS pass on the network, by going up the tree and applying f_R(n) for every non-leaf node n∈N on its RHS node representations (see Figure 3(a)).
    -- | These networks f_R(n) produce a node representation which is input into the parent’s rule network and so on until we reach the root node.
    -- | Once at the root node, we effectively have a fixed-dimensionality global tree representation ϕ(root) for the start symbol.
    let root_emb :: Tnsr '[1, m] = let
                traverseTree :: Expr -> Tnsr '[1, m] = \expr -> let
                        f = traverseTree
                    in case expr of
                        Paren _l xpr -> f xpr
                        Let _l _binds xpr -> f xpr
                        ExpTypeSig _l xpr _tp -> f xpr
                        -- Con _l _qname -> expr_slice
                        Var _l qname -> let idx = case qname of
                                                UnQual _l name -> case name of
                                                    Ident _l str -> symbolIdxs ! str
                                                    _ -> error $ "unexpected Name: " ++ show name
                                                _ -> error $ "unexpected QName: " ++ show qname
                                            in asUntyped (select'' 0 idx) conditioned'
                                            -- in select conditioned' @0 @idx  -- idx is run-time...
                        App _l _exp1 _exp2 -> let
                                tensors :: [Tnsr '[1, m]] = f <$> fnAppNodes expr
                                nnet = UntypedMLP.mlp $ lookupRule left_nnets $ nodeRule expr
                            in UnsafeMkTensor $ nnet $ F.cat 1 $ toDynamic <$> tensors
                        _ -> error $ "unexpected Expr: " ++ show expr
            in traverseTree ppt
    print $ "root_emb: " ++ show (shape' root_emb)

    -- perform a reverse-recursive pass starting from the root to assign a global tree representation to each node in the tree.
    -- | The problem is that this representation has lost any notion of tree position.
    -- | To solve this problem R3NN now does what is effectively a reverse-recursive pass which starts at the root node with ϕ(root) as input and moves towards the leaf nodes (see Figure 3(b)).
    -- | To compute the probability distribution over the set E, the R3NN computes a distributed representation for each leaf node that contains global tree information.
    -- | More concretely, we start with the root node representation ϕ(root) and use that as input into the rule network g_R(root) where R(root) is the production rule that is applied to the start symbol in the PPT.
    -- | This produces a representation ϕ′(c) for each RHS node c of R(root).
    -- | If c is a non-leaf node, we iteratively apply this procedure to c, i.e.,
    -- | process ϕ′(c) using g_R(c) to get representations ϕ′(cc) for every RHS node cc of R(c), etc.
    -- | If c is a leaf node, we now have a leaf representation ϕ′(c) which has an information path to ϕ(root) and thus to every other leaf node in the tree.
    -- | Once the reverse-recursive process is complete, we now have a distributed representation ϕ′(l) for every leaf node l which contains global tree information.
    -- | While ϕ(l1) and ϕ(l2) could be equal for leaf nodes which have the same symbol type,
    -- | ϕ′(l1) and ϕ′(l2) will not be equal even if they have the same symbol type because they are at different positions in the tree.
    -- note: order here *must* follow `findHolesExpr`!
    let node_embs :: Tnsr '[num_holes, m] = let
                traverseTree
                    :: Tnsr '[1, m]
                    -> Expr
                    -> [Tnsr '[1, m]]
                    = \ node_emb expr -> let
                        f = traverseTree node_emb
                    in case expr of
                        Paren _l xpr -> f xpr
                        Let _l _binds xpr -> f xpr
                        ExpTypeSig _l xpr _tp -> f xpr
                        -- Con _l _qname -> []  -- terminal leaf, discard embedding
                        Var _l qname -> case qname of
                            -- Special _l specialCon -> case specialCon of
                            --     ExprHole _l -> [node_emb]
                            --     _ -> []
                            UnQual _l name -> case name of
                                Ident _l str -> case str of
                                    "undefined" -> [node_emb]  -- hole i.e. non-terminal, gotta keep this
                                    _ -> []  -- terminal leaf, discard embedding
                                _ -> error $ "unexpected Name: " ++ show name
                            _ -> error $ "unexpected QName: " ++ show qname
                        App _l _exp1 _exp2 -> let
                                nnet = UntypedMLP.mlp $ lookupRule right_nnets $ nodeRule expr
                                -- tensor :: Tnsr '[1, q * m] =
                                --         asUntyped nnet node_emb
                                tensor :: D.Tensor = nnet $ toDynamic node_emb
                                -- (fn, args) = fnAppNodes expr
                                child_exprs :: [Expr] = fnAppNodes expr
                                -- split tensor into q tensors of '[1, m]...
                                -- TODO: ensure I split it right
                                q :: Int = length child_exprs
                                tensors :: [Tnsr '[1, m]] =
                                        UnsafeMkTensor <$> (unDim 0 . D.reshape [q, -1] $ tensor) --  . toDynamic   -- [q, m]
                                tensors' :: [Tnsr '[1, m]] =
                                        uncurry traverseTree =<< zip tensors child_exprs
                            in tensors'
                        _ -> error $ "unexpected Expr: " ++ show expr
            in 
                UnsafeMkTensor $ F.cat 0 $ toDynamic <$> traverseTree root_emb ppt
    print $ "node_embs: " ++ show (shape' node_embs)

    -- OPTIONAL
    -- | An additional improvement that was found to help was to add a bidirectional LSTM to process the global leaf representations right before calculating the scores. The LSTM hidden states are then used in the score calculation rather than the leaves themselves. This serves primarily to reduce the minimum length that information has to propagate between nodes in the tree. The R3NN can be seen as an extension and combination of several previous tree-based models, which were mainly developed in the context of natural language processing (Le & Zuidema, 2014; Paulus et al., 2014; Irsoy & Cardie, 2013).
    -- this should be an LSTM but for MLP an untyped implementation is available now so let's try that first...
    -- Tnsr '[batch_size, t, Dirs * H] =
    -- '[num_holes, m]
    -- scoreSpec :: LSTMSpec m H NumLayers Dir 'D.Float Dev,
    let node_embs' :: Tnsr '[num_holes, m] =
    -- let node_embs' :: Tnsr '[num_holes, Div m Dirs * Dirs] =
            -- asUntyped (UntypedMLP.mlp score_model) node_embs
            -- asUntyped to type-check m*2/2
            asUntyped (\t -> I.squeezeDim t 0) .
            -- asUntyped id .
            -- squeezeAll . -- unsafe, can squeeze out num_holes too
            -- reshape '[num_holes, Div m Dirs * Dirs] .
            -- reshape . 
            -- asUntyped (D.reshape . D.shape . toDynamic $ node_embs) . 
            -- fstOf3 . lstmWithDropout @'SequenceFirst score_model . unsqueeze @0 $ node_embs
            fstOf3 . lstmDynamicBatch @'SequenceFirst dropoutOn score_model . unsqueeze @0 $ node_embs
                    where dropoutOn = True
    print $ "node_embs': " ++ show (shape' node_embs')

    -- | 4.1.2 Expansion Probabilities

    -- | Given the global leaf representations ϕ′(l), we can now straightforwardly acquire scores for each e∈E.
    -- | For expansion e, let e.r be the expansion type (production rule r∈R that e applies) and let e.l be the leaf node l that e.r is applied to.
    -- | A valid expansion has two components: first the production rule used, and
    -- | second the position of the expanded leaf node relative to every other node
    -- | in the tree.
    -- | The score of an expansion is calculated using z_e=ϕ′(e.l)⋅ω(e.r).
    let scores :: Tnsr '[num_holes, rules] =
            matmul node_embs' . transpose @0 @1 $ Torch.Typed.Parameter.toDependent rule_emb
            -- matmul node_embs' . transpose @0 @1 . UnsafeMkTensor . D.toDependent $ rule_emb
            -- asUntyped (F.matmul $ toDynamic node_embs') . transpose @0 @1 . UnsafeMkTensor . D.toDependent $ rule_emb
    print $ "scores: " ++ show (shape' scores)

    return scores
    -- delay softmax in favor of combining them into `binary_cross_entropy_with_logits` for numerical stability (log-sum-exp trick)

    -- -- | probabilities to every valid expansion in the current PPT.
    -- -- | The probability of expansion e is simply the exponentiated normalized sum over all scores: π(e) = e^(z_e) / (∑_{e′∈E} e^{z_e′}).
    -- let hole_expansion_probs :: Tnsr '[num_holes, rules] =
    --         asUntyped softmaxAll scores
    -- print $ "hole_expansion_probs: " ++ show (shape' hole_expansion_probs)

    -- -- TODO: zero-mask non-compiling expansions? ensure penalized by loss?
    -- return hole_expansion_probs
