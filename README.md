# Marco

A Markov chains based poet bot.

## Build

Using cabal is recommended, although not necessarily required, in order to build the program.

### With cabal

```bash
cabal build
```

You will find the `marco` executable in _dist/build/marco/_.

### With GHC

```bash
cd src
ghc Main.hs -o marco
```

You will find the `marco` executable (unless renamed) in _src/_.

## Usage

Assuming the program has been built properly and the executable is called `marco`:

```bash
marco filename context_length max_length
```

Where

* *filename* is the (absolute or relative) path to a file to be used as the base text
* *context_length* is the amount of words to be considered as a "unit" in order to find the next suffix
* *max_length* is an *approximate* (not necessarily exact) upper bound for the generated text. In practice, the text's length may vary from 0 (which would be a really bad scenario that might imply that youre base text is not adecuate for this task) to (*max_length* + *context_length*).

## To-do list

* Improve performance
* Persistent memory, if required
* Bot
* Normalize better
* Exception handling
* Edge-case testing
* Exact version match in cabal
* Stack instead of cabal?
* Sample texts: inputs and outputs
* Better README: add description, motivation, etc!
