# Typed Neuro-Symbolic Program Synthesis for the Typed Lambda Calculus

[![Build Status](https://travis-ci.com/tycho01/synthesis.svg?branch=master)](https://travis-ci.com/tycho01/synthesis)

## Links

- introductory [slides](https://docs.google.com/presentation/d/1gS3sDgF7HPkiTnE9piQ6IDSFm6idGD7MaXalYzw9BC0/edit?usp=sharing)
- the accompanying [paper](https://github.com/tycho01/thesis), my AI master thesis at UvA
- the [Neuro-Symbolic Program Synthesis](https://arxiv.org/abs/1611.01855) paper I'm building on top of
- [spreadsheet](https://docs.google.com/spreadsheets/d/1uDA9suwASDzllxJZDt--wZ0ci7q4eJIfPcAw9qr18-U/edit?usp=sharing) used a bit in design
- Haddock [documentation](https://tycho01.github.io/synthesis/) for the codebase
- [roadmap](https://github.com/tycho01/synthesis/projects/1)

## Usage

You can build and run this project using [Nix](https://nixos.org/nix/) + [Cabal](https://www.haskell.org/cabal/).

``` sh
# install Nix, Cachix:
bash <(curl https://nixos.org/nix/install)
nix-env -iA cachix -f https://cachix.org/api/v1/install

# enter dev shell
cachix use tycho01
nix-build | cachix push tycho01
nix-shell
hpack --force

# basic commands
cabal v1-build
cabal v1-test
cabal v1-repl lib:synthesis
cabal v1-run generator   -- --help
cabal v1-run synthesizer -- --help

# command-line auto-complete
# bash
cabal v1-run generator   -- --bash-completion-script `cabal v1-exec which generator` >> ~/.bash_completion
cabal v1-run synthesizer -- --bash-completion-script `cabal v1-exec which synthesizer` >> ~/.bash_completion
# fish
cabal v1-run generator   -- --fish-completion-script (cabal v1-exec which generator) > ~/.config/fish/completions/generator.fish
cabal v1-run synthesizer -- --fish-completion-script (cabal v1-exec which synthesizer) > ~/.config/fish/completions/synthesizer.fish

# Generate documentation.
cabal v1-build --enable-documentation

# Profile
cabal v1-build --enable-profiling --ghc-options="-fno-prof-auto"
`cabal v1-exec which generator` +RTS -p

# viz
stack install ghc-prof-flamegraph
ghc-prof-flamegraph generator.prof
stack install profiterole
profiterole generator.prof
```

questions for NSPS authors:
- why not concatenate i/o features directly to symbol features?
- tanh activation: XCorrIO too, so for any LSTMs too?
- how does batching work in the R3NN?
