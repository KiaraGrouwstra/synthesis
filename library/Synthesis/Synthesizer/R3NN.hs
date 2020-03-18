{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Synthesis.Synthesizer.R3NN (
    r3nn,
) where

import Data.Word (Word64)
import Data.Bifunctor (second)
import Data.HashMap.Lazy (HashMap, fromList, (!), size, keys, lookup)
import Control.Monad ((=<<))
import Language.Haskell.Exts.Syntax
-- import Language.Haskell.Interpreter (Interpreter, lift, liftIO)
import GHC.TypeNats (KnownNat, Mod, Div, type (*), type (+), type (-))

import Torch.Typed.Tensor
import Torch.Typed.Functional
import Torch.Typed.NN
-- import Torch.Typed.Factories
import Torch.Typed.Aux
import Torch.Random (Generator)
import Torch.Typed.NN.Recurrent.LSTM
import Torch.HList
import qualified Torch.HList
import qualified Torch.NN                      as A
import qualified Torch.Functional              as F
import qualified Torch.Tensor                  as D
import qualified Torch.DType                   as D
import qualified Torch.Device                  as D
import qualified Torch.Functional.Internal     as D
import qualified Torch.Random                  as D

import Synthesis.Orphanage ()
import Synthesis.Data (Expr, Tp, L)
import Synthesis.Types (appFn, appArg, unQName)
import Synthesis.Utility (pp, pp_)
import Synthesis.Blocks (blockAsts)
import Synthesis.FindHoles (findHolesExpr, findIdentExpr, findFnAppExpr, findHolesExpr', findIdentExpr', findFnAppExpr', findTopFnAppExpr')
import Synthesis.Synthesizer.Utility
import qualified Synthesis.Synthesizer.Encoder as Encoder
import qualified Synthesis.Synthesizer.UntypedMLP as UntypedMLP
-- import qualified Synthesis.Synthesizer.TypedMLP as TypedMLP

-- TODO: consider which hyperparams have been / should be shared across networks
-- now shared between left/right nns, no clue how many layers or even hidden units I'd want
type HiddenFeatures0 = 20 -- ?
type HiddenFeatures1 = 20 -- ?
-- pre-score lstm
-- type NumLayers = 3 -- ?
-- | H is the topmost LSTM hidden dimension
-- type H = Div M Dirs -- needed for node_embs' with bidir lstm to still output a size that can be dot-product'd with symbol_expansions_emb
-- dropoutRate :: Double
-- dropoutRate = 0.0 -- drop-out not mentioned in NSPS

-- TODO: ensure differentiable? see https://hasktorch.github.io/hasktorch-0.2.0.0/html/Torch-Typed-Autograd.html#t:HasGrad

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
-- TODO: which state here should carry over across invocations?
    -- forall n -- rules num_leaves num_nodes num_holes
    -- . ( KnownNat n )
    -- . ( KnownNat n,
        -- KnownNat num_leaves,  -- Let L be the current leaf nodes of the PPT.
        -- KnownNat num_nodes,   -- Let N be the current non-leaf (rule) nodes of the PPT.??
        -- KnownNat num_holes,
        -- KnownNat rules)       -- Let E be the set of all valid expansions in a PPT.
    -- -> 
r3nn
    :: forall m n rules numHoles
    . ( KnownNat m )
    => HashMap String Int
    -> Tensor Dev 'D.Float '[Symbols, m]
    -> Tensor Dev 'D.Float '[rules, m]
    -> Expr
    -> Tensor Dev 'D.Float '[n, 2 * Dirs * Encoder.H * Encoder.T]
    -> IO (Tensor Dev 'D.Float '[numHoles, rules])
r3nn
    -- dsl
    variant_sizes
    symbol_emb              -- NSPS's phi
    symbol_expansions_emb   -- NSPS's omega
    ppt
    io_vec
    = do
    let n :: Int = shape' io_vec !! 0
    print $ "n: " ++ show n
    print $ "io_vec: " ++ show (shape' io_vec)
    let hiddenFeatures0 :: Int = natValI @HiddenFeatures0
    let hiddenFeatures1 :: Int = natValI @HiddenFeatures1
    -- let symbol_emb = toDynamic symbol_emb'

    -- | 5.2 CONDITIONING PROGRAM SEARCH ON EXAMPLE ENCODINGS
    -- | Once the I/O example encodings have been computed,
    -- | we can use them to perform conditional generation of the program tree using the R3NN model.
    -- | condition the PPT generation model using the I/O example encodings
    -- | Pre-conditioning: example encodings are concatenated to the encoding of each tree leaf,
    -- i/o features, identical for each leaf
    let io_vec' :: Tensor Dev 'D.Float '[n * 2 * Dirs * Encoder.H * Encoder.T] =
            asUntyped (D.reshape [n * 2 * dirs * Encoder.h * Encoder.t]) io_vec
            -- reshape '[n * 2 * Dirs * Encoder.H * Encoder.T] io_vec
    print $ "io_vec': " ++ show (shape' io_vec')
    -- since these extra features don't depend on the leaf node, already concatenate them to `symbol_emb` instead of per leaf (`leaf_embs`) like in NSPS so for dim `Symbols` instead of `NumLeaves`
    -- untyped as num_leaves is dynamic
    -- (toDynamic leaf_embs)
    -- let conditioned :: D.Tensor = F.cat 1 [leaf_embs, D.stack (replicate (shape' leaf_embs !! 0) (toDynamic io_vec')) 0]
    -- let conditioned :: D.Tensor = F.cat 1 [leaf_embs, D.stack (replicate (shape' leaf_embs !! 0) io_vec') 0]
    -- let stacked_io = D.stack (replicate symbols io_vec') 0
    -- print $ "stacked_io: " ++ show (shape' stacked_io)
    let conditioned :: Tensor Dev 'D.Float '[Symbols, m + n * 2 * Dirs * Encoder.H * Encoder.T] =
            UnsafeMkTensor $ F.cat 1 [toDynamic symbol_emb, stacked_io]
            where stacked_io = D.stack (toDynamic <$> replicate symbols io_vec') 0 -- n
    print $ "conditioned: " ++ show (shape' conditioned)
    -- | and then passed to a conditioning network before the bottom-up recursive pass over the program tree.
    -- The conditioning network can be either a multi-layer feedforward network,
    -- -- mlp <- A.sample (TypedMLP.MLPSpec
    -- --         @(m + n * 2 * Dirs * Encoder.H * Encoder.T) @m
    -- --         @H0 @H1
    -- --         @D.Float
    -- --         D.Device
    -- --         dropoutRate
    -- --     )
    -- -- let conditioned' :: Tensor Dev 'D.Float '[Symbols, m] = TypedMLP.mlp conditioned
    -- -- waaaait, can I really cram all that back into M??
    mlp_model <- A.sample (UntypedMLP.MLPSpec
            (m + n * 2 * dirs * Encoder.h * Encoder.t)
            h0
            h1
            m
        )
    let conditioned' :: Tensor Dev 'D.Float '[Symbols, m] =
            asUntyped (UntypedMLP.mlp mlp_model) conditioned
    -- or a bidirectional LSTM network running over tree leaves.
    -- Running an LSTM over tree leaves allows the model to learn more about the relative position of each leaf node in the tree.
    -- let conditioned' :: Tensor Dev 'D.Float '[Symbols, Dirs * H] = emb
    --     where
    --         -- htan activation?
    --         lstm :: LSTMWithInit m H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample $ LSTMWithZerosInitSpec $ LSTMSpec $ DropoutSpec $ dropoutRate
    --         let (emb, hidden, cell) :: (
    --                 Tensor Dev 'D.Float '[Symbols, 1, Dirs * H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, Symbols, H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, Symbols, H])
    --                     = lstmWithDropout @'BatchFirst lstm conditioned
    print $ "conditioned': " ++ show (shape' conditioned')

    -- For every production rule r∈R, a deep neural network f_r which takes as
    -- input a vector x∈R^Q⋅M, with Q being the number of symbols on the RHS of
    -- the production rule r, and outputs a vector y∈R^M.
    -- Therefore, the production-rule network f_r takes as input a concatenation
    -- of the distributed representations of each of its RHS symbols and produces
    -- a distributed representation for the LHS symbol.
    -- TODO: further check MLPMnist for usage
    -- TODO: does it even make sense to just randomly generate these?
    -- forall n' q . (KnownNat n', KnownNat q) => Tensor Dev 'D.Float '[n', q * m] -> IO (Tensor Dev 'D.Float '[n', m])
    expansion_left_nnets :: HashMap String (D.Tensor -> D.Tensor) <- let
        -- untyped as q is not static
        -- forall n' q . (KnownNat n', KnownNat q) => 
        -- Tensor Dev 'D.Float '[n', q * m] -> Tensor Dev 'D.Float '[n', m]
        mapper :: Int -> (IO (D.Tensor -> D.Tensor)) = \ q -> do
            spec :: UntypedMLP.MLP <- A.sample $ UntypedMLP.MLPSpec (q * m) hiddenFeatures0 hiddenFeatures1 m
            let mkNN :: D.Tensor -> D.Tensor = UntypedMLP.mlp spec
            return mkNN --  $ asUntyped mkNN
        in mapper `mapM` variant_sizes
    -- print $ "expansion_left_nnets: " ++ show (keys expansion_left_nnets)

    -- For every production rule r∈R, an additional deep neural network g_r which takes as input a vector x′∈R^M and outputs a vector y′∈R^Q⋅M.
    -- We can think of g_r as a reverse production-rule network that takes as input a vector representation of the LHS and produces a concatenation of the distributed representations of each of the rule’s RHS symbols.
    -- forall n' q . (KnownNat n', KnownNat q) => Tensor Dev 'D.Float '[n', m] -> IO (Tensor Dev 'D.Float '[n', q * m])
    expansion_right_nnets :: HashMap String (D.Tensor -> D.Tensor) <- let
        -- untyped as q is not static
        mapper :: Int -> IO (D.Tensor -> D.Tensor) = \ q -> do
            spec :: UntypedMLP.MLP <- A.sample $ UntypedMLP.MLPSpec m hiddenFeatures0 hiddenFeatures1 (q * m)
            let mkNN :: D.Tensor -> D.Tensor = UntypedMLP.mlp spec
            return $ mkNN --  $ asUntyped mkNN
        in mapper `mapM` variant_sizes
    -- print $ "expansion_right_nnets: " ++ show (keys expansion_right_nnets)

    -- | 4.1.1 Global Tree Information at the Leaves

    -- -- I'm now skipping this as I'm concatting i/o features straight to symbols while matching symbols is actually easier for me than matching leaves, given my Expr isn't uniquely identifiable (without proper SrcSpanInfo). this should simplify it will staying equivalent. I should test this later.
    -- -- Given a partial tree, the model first assigns a vector representation to each leaf node.
    -- -- | for every leaf node l∈L in the tree we retrieve its distributed representation ϕ(S(l)).
    -- let leaf_embs :: Tensor Dev 'D.Float '[NumLeaves, m] = UnsafeMkTensor $ F.cat 0 $ select' (toDynamic symbol_emb) 0 . symbolNum . leaf_symbol <$> leaves
    --                         where symbolNum = \case
    --                                 Hole -> 0
    --                                 Variable -> 1

    -- perform a recursive pass going up in the tree to assign a global tree representation to the root.
    -- | We now do a standard recursive bottom-to-top, RHS→LHS pass on the network, by going up the tree and applying f_R(n) for every non-leaf node n∈N on its RHS node representations (see Figure 3(a)).
    -- | These networks f_R(n) produce a node representation which is input into the parent’s rule network and so on until we reach the root node.
    -- | Once at the root node, we effectively have a fixed-dimensionality global tree representation ϕ(root) for the start symbol.
    -- print $ "nodeRule ppt: " ++ show (nodeRule ppt)
    -- my DSL doesn't currently distinguish symbols so just prep it up-front
    let expr_slice :: Tensor Dev 'D.Float '[1, m] =
            asUntyped (\t -> select' t 0 0) conditioned'  -- symbol_emb
            -- select @0 @0 conditioned'  -- this removes the dimension...
    
    let root_emb :: Tensor Dev 'D.Float '[1, m] = let
                traverseTree :: Expr -> Tensor Dev 'D.Float '[1, m] = \expr -> let
                        f = traverseTree
                    in case expr of
                        Paren _l xpr -> f xpr
                        Let _l _binds xpr -> f xpr
                        ExpTypeSig _l xpr _tp -> f xpr
                        Con _l _qname -> expr_slice
                        Var _l _qname -> expr_slice
                        App _l _exp1 _exp2 -> let
                                tensors :: [Tensor Dev 'D.Float '[1, m]] = f <$> fnAppNodes expr
                                nnet = lookupRule expansion_left_nnets $ nodeRule expr
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
    -- note: order here should follow `findHolesExpr`!
    let node_embs :: Tensor Dev 'D.Float '[numHoles, m] = let
                traverseTree
                    :: Tensor Dev 'D.Float '[1, m]
                    -> Expr
                    -> [Tensor Dev 'D.Float '[1, m]]
                    = \ node_emb expr -> let
                        f = traverseTree node_emb
                    in case expr of
                        Paren _l xpr -> f xpr
                        Let _l _binds xpr -> f xpr
                        ExpTypeSig _l xpr _tp -> f xpr
                        Con _l _qname -> []  -- terminal leaf, discard embedding
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
                                nnet = lookupRule expansion_right_nnets $ nodeRule expr
                                -- tensor :: Tensor Dev 'D.Float '[1, q * m] =
                                --         asUntyped nnet node_emb
                                tensor :: D.Tensor = nnet $ toDynamic node_emb
                                -- (fn, args) = fnAppNodes expr
                                -- child_exprs :: [Expr] = fn : args
                                child_exprs :: [Expr] = fnAppNodes expr
                                -- split tensor into q tensors of '[1, m]...
                                -- TODO: ensure I split it right
                                q :: Int = length child_exprs
                                tensors :: [Tensor Dev 'D.Float '[1, m]] =
                                        UnsafeMkTensor <$> (unDim 0 . D.reshape [q, m] $ tensor) --  . toDynamic
                                tensors' :: [Tensor Dev 'D.Float '[1, m]] =
                                        uncurry traverseTree =<< zip tensors child_exprs
                            in tensors'
                        _ -> error $ "unexpected Expr: " ++ show expr
            in 
                UnsafeMkTensor $ F.cat 0 $ toDynamic <$> traverseTree root_emb ppt
    print $ "node_embs: " ++ show (shape' node_embs)

    -- OPTIONAL
    -- | An additional improvement that was found to help was to add a bidirectional LSTM to process the global leaf representations right before calculating the scores. The LSTM hidden states are then used in the score calculation rather than the leaves themselves. This serves primarily to reduce the minimum length that information has to propagate between nodes in the tree. The R3NN can be seen as an extension and combination of several previous tree-based models, which were mainly developed in the context of natural language processing (Le & Zuidema, 2014; Paulus et al., 2014; Irsoy & Cardie, 2013).
    -- this should be an LSTM but for MLP an untyped implementation is available now so let's try that first...
    mlp_model_score <- A.sample (UntypedMLP.MLPSpec
            m
            h0
            h1
            m
        )
    let node_embs' :: Tensor Dev 'D.Float '[numHoles, m] =
            asUntyped (UntypedMLP.mlp mlp_model_score) node_embs
    -- damnit, given numHoles is dynamic (may differ by expression, unless I fix some max?),
    -- I would need to switch to an untyped LSTM but that doesn't seem to be built-in right now...
    -- TODO: convert from 2d to 3d for input and back for output...
    --  :: Tensor Dev 'D.Float '[numHoles, Dirs * H]
    -- let node_embs' :: D.Tensor = do
    --         -- htan activation?
    --         lstm :: LSTMWithInit m H NumLayers Dir 'ConstantInitialization 'D.Float Dev <- A.sample . LSTMWithZerosInitSpec . LSTMSpec . DropoutSpec $ dropoutRate
    --         let (emb, hidden, cell) :: (
    --                 Tensor Dev 'D.Float '[numHoles, 1, Dirs * H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, numHoles, H],
    --                 Tensor Dev 'D.Float '[Dirs * NumLayers, numHoles, H])
    --                     = lstmWithDropout @'BatchFirst lstm $ UnsafeMkTensor node_embs
    --         return . toDynamic $ emb

    -- | 4.1.2 Expansion Probabilities

    -- | Given the global leaf representations ϕ′(l), we can now straightforwardly acquire scores for each e∈E.
    -- | For expansion e, let e.r be the expansion type (production rule r∈R that e applies) and let e.l be the leaf node l that e.r is applied to.
    -- | A valid expansion has two components: first the production rule used, and
    -- | second the position of the expanded leaf node relative to every other node
    -- | in the tree.
    -- | The score of an expansion is calculated using z_e=ϕ′(e.l)⋅ω(e.r).
    -- TODO: filter to holes i.e. non-terminal leaf nodes, cuz non-holes cannot be expanded...
    let scores :: Tensor Dev 'D.Float '[numHoles, rules] =
            asUntyped (F.matmul $ toDynamic node_embs') $ transpose @0 @1 symbol_expansions_emb
    print $ "scores: " ++ show (shape' scores)

    -- | probabilities to every valid expansion in the current PPT.
    -- | The probability of expansion e is simply the exponentiated normalized sum over all scores: π(e) = e^(z_e) / (∑_{e′∈E} e^z * e′).
    let hole_expansion_probs :: Tensor Dev 'D.Float '[numHoles, rules] =
            asUntyped (F.softmax 1) scores
    -- nodeRule :: Expr -> String
    print $ "hole_expansion_probs: " ++ show (shape' hole_expansion_probs)

    -- TODO: zero-mask non-compiling expansions? ensure penalized by loss?
    return hole_expansion_probs
