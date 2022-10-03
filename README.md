# Moonad

_Moonad_ is an interpreter for the esolang _[COW](https://bigzaphod.github.io/COW/)_. I have written it for fun and to improve my Haskell skills, especially of `Parsec` and the monad transformers `ReaderT` and `StateT`.

## Usage

### Installation

First [install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/). Then compile the project:

``` shell
stack build
```

### Execution

To run a file containing _COW_ code (e.g. one of the [examples](https://github.com/BigZaphod/COW/blob/master/examples/i.cow) in the original project):

``` shell
stack exec Moonad-exe <filename>
```

## Acknowledgments

- Sean Heber (BigZaphod) for creating _COW_
