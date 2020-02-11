# synthesis

[![Build Status](https://travis-ci.com/tycho01/synthesis.svg?branch=master)](https://travis-ci.com/tycho01/synthesis)

## Links

- introductory [slides](https://docs.google.com/presentation/d/1gS3sDgF7HPkiTnE9piQ6IDSFm6idGD7MaXalYzw9BC0/edit?usp=sharing)
- the accompanying [paper](https://github.com/tycho01/thesis), my AI master thesis at UvA
- [spreadsheet](https://docs.google.com/spreadsheets/d/1uDA9suwASDzllxJZDt--wZ0ci7q4eJIfPcAw9qr18-U/edit?usp=sharing) used a bit in design
- Haddock [documentation](https://tycho01.github.io/synthesis/) for the codebase
- [roadmap](https://github.com/tycho01/synthesis/projects/1)

## Usage

You can build and run this project using either [Stack](https://docs.haskellstack.org/) or [Docker](https://www.docker.com/).

``` sh
# basic Stack commands
stack build
stack test

# generator-specific
stack exec -- generator --help
stack exec -- generator
stack build --fast --exec "generator --help"

# synthesizer-specific
stack exec -- synthesizer --help
stack exec -- synthesizer
stack build --fast --exec "synthesizer --help"

# command-line auto-complete
# bash
stack exec -- generator --bash-completion-script `stack exec which generator` >> ~/.bash_completion
stack exec -- synthesizer --bash-completion-script `stack exec which synthesizer` >> ~/.bash_completion
# fish
stack exec -- generator --fish-completion-script (stack exec which generator) > ~/.config/fish/completions/generator.fish
stack exec -- synthesizer --fish-completion-script (stack exec which synthesizer) > ~/.config/fish/completions/synthesizer.fish

# Continuously build by Nix, run tests and execute
stack --nix test --file-watch --fast --exec generator

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock

# Profile

# cabal
cabal new-build --enable-profiling --ghc-options="-fno-prof-auto"
generator +RTS -p

# stack
stack build --profile --ghc-options="-fno-prof-auto"
stack exec -- generator +RTS -p -RTS

# viz
stack install ghc-prof-flamegraph
ghc-prof-flamegraph generator.prof
stack install profiterole
profiterole generator.prof

# Docker: install deps from base image; rebuild top image on code changes.
# this still sucks but Stack hates volume mounting. :(
docker build -t synthesis -f ./docker/base/Dockerfile .
docker build -t synthesis -f ./docker/top/Dockerfile .
docker run -ti synthesis stack test --exec generator
```
