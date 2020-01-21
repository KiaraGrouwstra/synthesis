import torch
from .xcorrio import XCorrIONet
from .r3nn import R3NN

# type: DSL
"""
A DSL can be considered a context-free grammar with a start symbol S and a
set of non-terminals with corresponding expansion rules. The (partial)
grammar derivations or trees correspond to (partial) programs.
"""

# type: PPT
"""
We define a program t-steps into construction as a partial program tree (PPT)
(see Figure 3 for a visual depiction).
A PPT has two types of nodes:
- leaf (symbol) nodes
- inner non-leaf (rule) nodes
A leaf node represents a symbol, whether non-terminal or terminal.
An inner non-leaf node represents a particular production rule of the DSL,
where the number of children of the non-leaf node is equivalent to
the arity of the RHS of the rule it represents.
A PPT is called a program tree (PT) whenever all the leaves of the tree are terminal symbols.
Such a tree represents a completed program under the DSL and can be executed.
We define an expansion as the valid application of a specific production rule
(e → e op2 e) to a specific non-terminal leaf node within a PPT (leaf with symbol e).
We refer to the specific production rule that an expansion is derived from as the expansion type.
It can be seen that if there exist two leaf nodes (l1 and l2) with the same
symbol then for every expansion specific to l1 there exists an expansion
specific to l2 with the same type.
"""

def program_sampler(dsl, input_gen_rules):
    pass
    # sample_programs(dsl)
    # gen_io_samples(regex)
    # return: samples


def sample_programs(dsl):
    """
    uniformly sample programs from the DSL, then use a
    rule-based strategy to compute well-formed input strings
    that satisfy the pre-conditions of the programs.
    """
    pass


def gen_io_samples(regex):
    """
    The corresponding output strings are obtained
    by running the programs on the input strings.
    """
    pass


def io_encoder(input, output):
    """
    The generative process is conditioned on a set of input-output examples to
    learn a program that is consistent with this set of examples. We experiment
    with multiple input-output encoders including an LSTM encoder that
    concatenates the hidden vectors of two deep bidirectional LSTM networks for
    input and output strings in the examples, and a Cross Correlation encoder
    that computes the cross correlation between the LSTM tensor representations
    of input and output strings in the examples.
    This vector is then used as an additional input in the R3NN model to
    condition the generative model.

    5.1 ENCODING INPUT/OUTPUT EXAMPLES

    There are two types of information that string manipulation programs need to extract from input-output examples:
    1) constant strings, such as “@domain.com” or “.”, which appear in all output examples;
    2) substring indices in input where the index might be further defined by a regular expression.
    These indices determine which parts of the input are also present in the output.
    To simplify the DSL, we assume that there is a fixed finite universe of possible constant strings that could appear in programs.
    Therefore we focus on extracting the second type of information, the substring indices.

    In earlier hand-engineered systems such as FlashFill, this information was extracted from the input-output strings by running the Longest Common Substring algorithm, a dynamic programming algorithm that efficiently finds matching substrings in string pairs.
    To extract substrings, FlashFill runs LCS on every input-output string pair in the I/O set to get a set of substring candidates.
    It then takes the entire set of substring candidates and simply tries every possible regex and constant index that can be used at substring boundaries, exhaustively searching for the one which is the most “general”, where generality is specified by hand-engineered heuristics.

    In contrast to these previous methods, instead of of hand-designing a complicated algorithm to extract regex-based substrings, we develop neural network based architectures that are capable of learning to extract and produce continuous representations of the likely regular expressions given input/output strings.

    5.1.1 Baseline LSTM encoder

    Our first I/O encoding network involves running two separate deep bidirectional LSTM networks for processing the input and the output string in each example pair.
    For each pair, it then concatenates the topmost hidden representation at every time step to produce a 4HT-dimensional feature vector per I/O pair, where T is the maximum string length for any input or output string, and H is the topmost LSTM hidden dimension.

    We then concatenate the encoding vectors across all I/O pairs to get a vector representation of the entire I/O set.
    This encoding is conceptually straightforward and has very little prior knowledge about what operations are being performed over the strings, i.e., substring, constant, etc., which might make it difficult to discover substring indices, especially the ones based on regular expressions.

    5.1.2 Cross Correlation encoder
    """
    pass


def NSPS(dsl):
    """
    Neuro-Symbolic program synthesis: encode neural search over the space of
    programs defined using a Domain-Specific Language (DSL), see NSPS figure 2.
    @return a synthesis algorithm A such that given
        a set of input-output example, {(i1, o1),... ,(in, on)},
        A returns a program P∈L that conforms to the input-output examples,
        i.e., ∀j : 1 ≤ j ≤ n P(ij ) = oj.

    A naïve way to perform a search over the programs in a DSL is to start from
    the start symbol S and then randomly choose non-terminals to expand with
    randomly chosen expansion rules until reaching a derivation with only terminals.
    We, instead, learn a generative model over partial derivations in the DSL
    that assigns probabilities to different non-terminals in a partial derivation
    and corresponding expansions to guide the search for complete derivations.
    """
    pass

def experiment():
    """
    see section 6
    """
    pass

# regular expression-based syntactic string transformations, using a DSL based on the one used by FlashFill (Gulwani, 2011; Gulwani et al., 2012)
# 238 real-world FlashFill benchmarks.
# altered from original FlashFill task, see paper (figure 7, 1b, section 2)
# python Gulwani flashfill implementations:
# - https://github.com/minori5214/programming-by-example
# - https://github.com/jrkoenig/stoke-driver
# - https://github.com/arkaaloke/BuildingsAliveAutoMetadata


def main():
    # N is batch size; D_in is input dimension;
    # H is hidden dimension; D_out is output dimension.
    N, D_in, H, D_out = 64, 1000, 100, 10

    # Create random Tensors to hold inputs and outputs
    x = torch.randn(N, D_in)
    y = torch.randn(N, D_out)

    # Construct our model by instantiating the class defined above
    model = XCorrIONet()

    # Construct our loss function and an Optimizer. The call to model.parameters()
    # in the SGD constructor will contain the learnable parameters of the two
    # nn.Linear modules which are members of the model.
    criterion = torch.nn.MSELoss(reduction="sum")
    optimizer = torch.optim.SGD(model.parameters(), lr=1e-4)
    for t in range(500):
        # Forward pass: Compute predicted y by passing x to the model
        y_pred = model(x)

        # Compute and print loss
        loss = criterion(y_pred, y)
        if t % 100 == 99:
            print(t, loss.item())

        # Zero gradients, perform a backward pass, and update the weights.
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()


if __name__ == "__main__":
    main()
