# [synthesis](https://tycho01.github.io/synthesis/)

Thanks for starting a project with Haskeleton! If you haven't heard of it
before, I suggest reading the introductory blog post. You can find it here:
<http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/>.

Before you get started, there are a few things that this template couldn't
provide for you. You should:

-   Add a synopsis to `package.yaml`. It should be a short (one sentence)
    explanation of your project.

-   Add a description to `package.yaml`. This can be whatever you want it to
    be.

-   Add a category to `package.yaml`. A list of categories is available on
    Hackage at <http://hackage.haskell.org/packages>.

-   Rename `library/Example.hs` to whatever you want your top-level module to
    be called. Typically this is the same as your package name but in
    `CamelCase` instead of `kebab-case`.

    -   Don't forget to rename the reference to it in
        `executable/Main.hs`!

Once you've done that, start working on your project with the Stack commands
you know and love.

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

Thanks again, and happy hacking!

[synthesis]: https://github.com/githubuser/synthesis
