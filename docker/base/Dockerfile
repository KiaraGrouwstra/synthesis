FROM haskell
RUN stack setup
WORKDIR /root
RUN stack install base haskell-src-exts hint containers unordered-containers ghc-lib-parser template-haskell ghc-lib random hashable hslogger tasty tasty-hspec HUnit tasty-hunit
# COPY . /root
# RUN cd /root && stack install
