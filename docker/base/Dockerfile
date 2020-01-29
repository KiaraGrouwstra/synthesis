FROM haskell
RUN stack setup
WORKDIR /root
RUN stack install base haskell-src-exts hint QuickCheck containers unordered-containers ghc-lib-parser template-haskell ghc-lib random transformers text hashable split hslogger dump tasty tasty-hspec HUnit tasty-hunit
# COPY . /root
# RUN cd /root && stack install
