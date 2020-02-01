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
stack exec -- synthesis

# Continuously build by Nix, run tests and execute
stack --nix test --file-watch --fast --exec synthesis

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock

# Run
stack exec -- synthesis

# Docker: install deps from base image; rebuild top image on code changes.
# this still sucks but Stack hates volume mounting. :(
docker build -t synthesis -f ./docker/base/Dockerfile .
docker build -t synthesis -f ./docker/top/Dockerfile .
docker run -ti synthesis stack test --exec synthesis
```
