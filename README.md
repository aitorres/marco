# Marco

A Markov chains based poet bot.

## Changelog

The current version is **version 0.2.0.0**. [Click here to read the changelog](CHANGELOG.md).

## Build

Using cabal is recommended, although not necessarily required, in order to build the program.

### With cabal

```bash
cabal build
```

You can run the program with `cabal run`, or install it into your setup with `cabal install` and then run it with `marco`.

### With GHC

```bash
cd src
ghc Main.hs -o marco
```

You will find the `marco` executable (unless renamed) in _src/_.

## Usage

Assuming the program has been built properly and the executable is called `marco` (replace `marco` with `cabal run` if necessary):

### Training the algorithm

```bash
marco train foldername context_length
```

Where

* *foldername* is the (absolute or relative) path to a folder with the files to be used as the base texts
* *context_length* is the amount of words to be considered as a "unit" in order to find the next suffix

For training to be successful, you need to save one or several `.txt` files within the specified folder (we suggest using the `data` folder in the repository).

### Generating text

```bash
marco generate max_length
```

* *max_length* is an *approximate* (not necessarily exact) upper bound for the generated text. In practice, the text's length may vary from 0 (which would be a really bad scenario that might imply that youre base text is not adecuate for this task) to (*max_length* + *context_length*).

## To-do list

* Improve performance
* Bot
* Normalize better
* Exception handling
* Edge-case testing
* Exact version match in cabal
* Stack instead of cabal?
* Sample texts: inputs and outputs
* Better README: add description, motivation, etc!
