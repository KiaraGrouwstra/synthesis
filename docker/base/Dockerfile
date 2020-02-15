FROM tensorflow/haskell:v0
WORKDIR /root
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack install base haskell-src-exts hint containers unordered-containers ghc-lib-parser ghc-lib random hashable hslogger tasty tasty-hspec HUnit tasty-hunit
RUN PATH=$PATH:/root/.local/bin >> ~/.bashrc
# COPY . /root
# RUN cd /root && stack install
