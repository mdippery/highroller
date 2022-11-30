# High Roller

> A D20 dice roll simulator

## Building

You can build the project using [Stack]:

    stack build

(The `.cabal` file is included for those that want to use [Cabal] to build the
project instead.)

## Testing

It is easy to run tests and generate a test coverage report:

    stack test --coverage

You can create an easy link to the coverage reports:

    mkdir -p docs
    pushd docs
    ln -s $(stack path --local-hpc-root) hpc
    popd

Open `docs/hpc/index.html` to view the coverage reports.

## Documentation

Haddock documentation can be created using Stack:

    stack haddock

You can create an easy link to this documentation to open it quickly:

    mkdir -p docs
    pushd docs
    ln -s $(stack path --local-doc-root) api
    popd

Open `docs/api/index.html` to view the documentation.

[Cabal]: https://www.haskell.org/cabal/
[Stack]: https://docs.haskellstack.org/en/stable/
