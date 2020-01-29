# [synthesis](https://tycho01.github.io/synthesis/)

## Usage

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
