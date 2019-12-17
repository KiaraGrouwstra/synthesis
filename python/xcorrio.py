import torch


class XCorrIONet(torch.nn.Module):
    """
    cross correlation I/O network (Parisotto et al.):
    given a set of input-output examples, produces a
    continuous representation of the set of I/O examples.

    5.1.2 Cross Correlation encoder

    To help the model discover input substrings that are copied to the output, we designed an novel I/O example encoder to compute the cross correlation between each input and output example representation.
    We used the two output tensors of the LSTM encoder (discussed above) as inputs to this encoder.
    For each example pair, we first slide the output feature block over the input feature block and compute the dot product between the respective position representation.
    Then, we sum over all overlapping time steps.
    Features of all pairs are then concatenated to form a 2∗(T−1)-dimensional vector encoding for all example pairs.
    There are 2∗(T−1) possible alignments in total between input and output feature blocks.
    We also designed the following variants of this encoder.

    Diffused Cross Correlation Encoder:
    This encoder is identical to the Cross Correlation encoder except that instead of summing over overlapping time steps after the element-wise dot product, we simply concatenate the vectors corresponding to all time steps, resulting in a final representation that contains 2∗(T−1)∗T features for each example pair.

    LSTM-Sum Cross Correlation Encoder:
    In this variant of the Cross Correlation encoder, instead of doing an element-wise dot product, we run a bidirectional LSTM over the concatenated feature blocks of each alignment.
    We represent each alignment by the LSTM hidden representation of the final time step leading to a total of 2∗H∗2∗(T−1) features for each example pair.

    Augmented Diffused Cross Correlation Encoder:
    For this encoder, the output of each character position of the Diffused Cross Correlation encoder is combined with the character embedding at this position, then a basic LSTM encoder is run over the combined features to extract a 4∗H-dimensional vector for both the input and the output streams.
    The LSTM encoder output is then concatenated with the output of the Diffused Cross Correlation encoder forming a (4∗H+T∗(T−1))-dimensional feature vector for each example pair.
    """

    def __init__(self):
        super(XCorrIONet, self).__init__()

    def forward(self, x):
        return x
