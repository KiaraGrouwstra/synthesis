import torch


class R3NN(torch.nn.Module):
    """
    Recursive Reverse-Recursive Neural Network (R3NN) (Parisotto et al.):
    given the continuous representation of the examples, synthesizes
    a program by incrementally expanding partial programs.
    R3NN employs a tree-based neural architecture that sequentially
    constructs a parse tree by selecting which non-terminal symbol
    to expand using rules from a context-free grammar (i.e., the DSL).

    Our generative model uses a Recursive-Reverse-Recursive Neural Network
    (R3NN) to encode partial trees (derivations) in L, where each node in the
    partial tree encodes global information about every other node in the tree.
    The model assigns a vector representation for every symbol and every
    expansion rule in the grammar. Given a partial tree, the model first
    assigns a vector representation to each leaf node, and then performs a
    recursive pass going up in the tree to assign a global tree representation
    to the root. It then performs a reverse-recursive pass starting from the
    root to assign a global tree representation to each node in the tree.

    4.1 RECURSIVE-REVERSE-RECURSIVE NEURAL NETWORK

    In order to define a generation model over PPTs, we need an efficient way
    of assigning probabilities to every valid expansion in the current PPT.
    A valid expansion has two components: first the production rule used, and
    second the position of the expanded leaf node relative to every other node
    in the tree.
    To account for the first component, a separate distributed representation
    for each production rule is maintained.
    The second component is handled using an architecture where the forward
    propagation resembles belief propagation on trees, allowing a notion of
    global tree state at every node within the tree.
    A given expansion probability is then calculated as being proportional to
    the inner product between the production rule representation and the
    global-tree representation of the leaf-level non-terminal node.
    We now describe the design of this architecture in more detail.

    The R3NN has the following parameters for the grammar described by a DSL (see Figure 3):

    For every symbol s∈S, an M-dimensional representation ϕ(s)∈RM.
    For every production rule r∈R, an M−dimensional representation ω(r)∈RM.
    For every production rule r∈R, a deep neural network fr which takes as
    input a vector x∈RQ⋅M, with Q being the number of symbols on the RHS of
    the production rule r, and outputs a vector y∈RM.
    Therefore, the production-rule network fr takes as input a concatenation
    of the distributed representations of each of its RHS symbols and produces
    a distributed representation for the LHS symbol.

    For every production rule r∈R, an additional deep neural network gr which takes as input a vector x′∈RM and outputs a vector y′∈RQ⋅M.
    We can think of gr as a reverse production-rule network that takes as input a vector representation of the LHS and produces a concatenation of the distributed representations of each of the rule’s RHS symbols.

    Let E be the set of all valid expansions in a PPT T, let L be the current leaf nodes of T and N be the current non-leaf (rule) nodes of T.
    Let S(l) be the symbol of leaf l∈L and R(n) represent the production rule of non-leaf node n∈N.

    (a) Recursive pass	(b) Reverse-Recursive pass
    Figure 3: (a) The initial recursive pass of the R3NN. (b) The reverse-recursive pass of the R3NN where the input is the output of the previous recursive pass.

    4.1.1 Global Tree Information at the Leaves

    To compute the probability distribution over the set E, the R3NN first computes a distributed representation for each leaf node that contains global tree information.
    To accomplish this, for every leaf node l∈L in the tree we retrieve its distributed representation ϕ(S(l)).
    We now do a standard recursive bottom-to-top, RHS→LHS pass on the network, by going up the tree and applying fR(n) for every non-leaf node n∈N on its RHS node representations (see Figure 3(a)).
    These networks fR(n) produce a node representation which is input into the parent’s rule network and so on until we reach the root node.

    Once at the root node, we effectively have a fixed-dimensionality global tree representation ϕ(root) for the start symbol.
    The problem is that this representation has lost any notion of tree position.
    To solve this problem R3NN now does what is effectively a reverse-recursive pass which starts at the root node with ϕ(root) as input and moves towards the leaf nodes (see Figure 3(b)).

    More concretely, we start with the root node representation ϕ(root) and use that as input into the rule network gR(root) where R(root) is the production rule that is applied to the start symbol in T.
    This produces a representation ϕ′(c) for each RHS node c of R(root).
    If c is a non-leaf node, we iteratively apply this procedure to c, i.e., process ϕ′(c) using gR(c) to get representations ϕ′(cc) for every RHS node cc of R(c), etc.
    If c is a leaf node, we now have a leaf representation ϕ′(c) which has an information path to ϕ(root) and thus to every other leaf node in the tree.
    Once the reverse-recursive process is complete, we now have a distributed representation ϕ′(l) for every leaf node l which contains global tree information.
    While ϕ(l1) and ϕ(l2) could be equal for leaf nodes which have the same symbol type, ϕ′(l1) and ϕ′(l2) will not be equal even if they have the same symbol type because they are at different positions in the tree.

    4.1.2 Expansion Probabilities

    Given the global leaf representations ϕ′(l), we can now straightforwardly acquire scores for each e∈E.
    For expansion e, let e.r be the expansion type (production rule r∈R that e applies) and let e.l be the leaf node l that e.r is applied to. ze=ϕ′(e.l)⋅ω(e.r)
    The score of an expansion is calculated using ze=ϕ′(e.l)⋅ω(e.r).
    The probability of expansion e is simply the exponentiated normalized sum over all scores: π(e)=eze∑e′∈Eeze′.

    An additional improvement that was found to help was to add a bidirectional LSTM to process the global leaf representations right before calculating the scores. The LSTM hidden states are then used in the score calculation rather than the leaves themselves. This serves primarily to reduce the minimum length that information has to propagate between nodes in the tree. The R3NN can be seen as an extension and combination of several previous tree-based models, which were mainly developed in the context of natural language processing (Le & Zuidema, 2014; Paulus et al., 2014; Irsoy & Cardie, 2013).

    5.2 CONDITIONING PROGRAM SEARCH ON EXAMPLE ENCODINGS

    Once the I/O example encodings have been computed, we can use them to perform conditional generation of the program tree using the R3NN model.
    There are a number of ways in which the PPT generation model can be conditioned using the I/O example encodings depending on where the I/O example information is inserted in the R3NN model.
    We investigated three locations to inject example encodings:

    1) Pre-conditioning (worked best):
    where example encodings are concatenated to the encoding of each tree leaf, and then passed to a conditioning network before the bottom-up recursive pass over the program tree.
    The conditioning network can be either a multi-layer feedforward network, or a bidirectional LSTM network running over tree leaves.
    Running an LSTM over tree leaves allows the model to learn more about the relative position of each leaf node in the tree.

    2) Post-conditioning:
    After the reverse-recursive pass, example encodings are concatenated to the updated representation of each tree leaf and then fed to a conditioning network before computing the expansion scores.

    3) Root-conditioning:
    After the recursive pass over the tree, the root encoding is concatenated to the example encodings and passed to a conditioning network.
    The updated root representation is then used to drive the reverse-recursive pass.
    """

    def __init__(self):
        super(R3NN, self).__init__()

    def forward(self, dsl, embedded_samples, tree):
        return tree
