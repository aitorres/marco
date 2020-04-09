# Marco

A Markov-chains based text generator (currently a work in progress).

Currently at **version v0.2.0.2**. [Click here to read the changelog](CHANGELOG.md).

## Build

We recommend to get and use **stack** in order to build, run and install the project.

### With stack

```bash
stack build
```

You can run the program with `stack run`, or install it into your setup with `stack install` and then run it with `marco`.

### With GHC

In case you don't have **stack** (note that this is not supported and might not work).

```bash
cd src
ghc Main.hs -o marco
```

You will find the `marco` executable in _src/_.

## Usage

Assuming the program has been built properly and the executable is called `marco` (replace `marco` with `stack run` if necessary):

### Training the algorithm

```bash
marco train <foldername> <context_length>
```

Where

- *foldername* is the (absolute or relative) path to a folder with the files to be used as the base texts
- *context_length* is the amount of words to be considered as a "unit" in order to find the next suffix

For training to be successful, you need to save one or several `.txt` files within the specified folder (we suggest using the `data` folder in the repository).

### Generating text

```bash
marco generate <max_length>
```

Where

- *max_length* is an upper bound for the generated text length (i.e. an *approximate*, not necessarily exact, max length). In practice, the text's length may vary from 0 (which would be a really bad scenario that might imply that your base texts are not adecuate for the task) to (*max_length* + *context_length*).

## To-do list

What follows is a non-exhaustive list of possible features to be added to `marco`:

- Improved performance, particularly in training
- An easy-to-use API to integrate text generation into other programs (e.g. bots)
- Better normalization algorithms
- The ability to define the first token to be used in text generation
- Code refactoring
- Exception handling
- Edge-case testing
- Sample texts: inputs and outputs
- Improved README: add description, motivation, etc!
