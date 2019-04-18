{- |
Module      :   Markov
Description :   Basic implementation of a Markov Chain algorithm for text
Copyright   :   (c) Andrés Ignacio Torres
License     :   MIT
Maintainer  :   andresitorresm@gmail.com
Stability   :   experimental

Basic implementation of an arbitrary-length Markov Chain algorithm.
-}

module Markov where

import Data.List

{-|
    Given an array of strings (words) and a context length,
    returns an array with each possible prefix (as an array) and the
    corresponding suffix.
-}
getSuffixArray :: [String] -> Int -> [([String], String)]
getSuffixArray [] _ = []
getSuffixArray words n =
  let enoughWords = length words > n in
    if enoughWords then
      let prefix = take n words
          suffix = words !! (n)
          remainingWords = tail words
      in  (prefix, suffix) : getSuffixArray remainingWords n
    else []
