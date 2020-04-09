# Revision history for marco

## v0.2.0.2 -- 2020-04-08

- `marco` now uses **stack** instead of just **cabal**.
- Improved README.

## v0.2.0.1 -- 2019-04-29

- Reduces unnecesary application of a normalization function.

## v0.2.0.0 -- 2019-04-28

- Modifies data structures to make them derive Read and Show, in order to easily keep record of them as files.
- Separates the training and content generation logic, in order to be able to train once and generate text several times with the same training dataset.
- Allows program to be trained with more than one text file, all conveniently stored within one folder.

## v0.1.0.0  -- 2019-04-18

- First version released! Works, but needs a lot of refactoring. This is pretty much an MVP.
