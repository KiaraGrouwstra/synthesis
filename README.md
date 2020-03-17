# synthesis

[![Build Status](https://travis-ci.com/tycho01/synthesis.svg?branch=master)](https://travis-ci.com/tycho01/synthesis)

## Links

- introductory [slides](https://docs.google.com/presentation/d/1gS3sDgF7HPkiTnE9piQ6IDSFm6idGD7MaXalYzw9BC0/edit?usp=sharing)
- the accompanying [paper](https://github.com/tycho01/thesis), my AI master thesis at UvA
- [spreadsheet](https://docs.google.com/spreadsheets/d/1uDA9suwASDzllxJZDt--wZ0ci7q4eJIfPcAw9qr18-U/edit?usp=sharing) used a bit in design
- Haddock [documentation](https://tycho01.github.io/synthesis/) for the codebase
- [roadmap](https://github.com/tycho01/synthesis/projects/1)

## Usage

You can build and run this project using [Nix](https://nixos.org/nix/) + [Cabal](https://wwwhaskell.org/cabal/).

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
cabal new-build
cabal new-test
cabal new-repl lib:synthesis
cabal new-run generator   -- --help
cabal new-run synthesizer -- --help

# command-line auto-complete
# bash
cabal new-run generator   -- --bash-completion-script `cabal new-exec which generator` >> ~/.bash_completion
cabal new-run synthesizer -- --bash-completion-script `cabal new-exec which synthesizer` >> ~/.bash_completion
# fish
cabal new-run generator   -- --fish-completion-script (cabal new-exec which generator) > ~/.config/fish/completions/generator.fish
cabal new-run synthesizer -- --fish-completion-script (cabal new-exec which synthesizer) > ~/.config/fish/completions/synthesizer.fish

# Generate documentation.
cabal new-build --enable-documentation

# Profile
cabal new-build --enable-profiling --ghc-options="-fno-prof-auto"
`cabal new-exec which generator` +RTS -p

# viz
stack install ghc-prof-flamegraph
ghc-prof-flamegraph generator.prof
stack install profiterole
profiterole generator.prof
```

## TODO
- figure out how to use the resulting tensor -> test
    - get untyped lstm
- filter to holes i.e. non-terminal leaf nodes, cuz non-holes cannot be expanded...
- 4.1.1 / 5.2: ensure understanding
