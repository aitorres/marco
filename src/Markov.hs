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

import Data.Char
import Data.List.Split


{-|
  Given an array of strings (words) and a context length,
  returns an array with each possible prefix (as an array) and the
  corresponding suffix.
-}
getSuffixArray :: [String] -> Int -> [([String], String)]
getSuffixArray [] _ = []
getSuffixArray words n
  | length words > n =
    let prefix = take n words
        suffix = words !! (n)
        remainingWords = tail words
    in  (prefix, suffix) : getSuffixArray remainingWords n
  | otherwise = []

{-|
  Given a suffix array, "compresses" the array in order to find common prefixes
  and associate all of the suffixes to them.
-}
compactSuffixArray :: [([String], String)] -> [([String], [String])]
compactSuffixArray arr =
  let denseSuffixArray = [(prefix, findAllSuffixes arr prefix) | (prefix, _) <- arr]
      dropDuplicates x =
        dropDupsAux x []
      dropDupsAux [] _ = []
      dropDupsAux (x:xs) seen
        | elem x seen = dropDupsAux xs seen
        | otherwise = x : dropDupsAux xs (x:seen)
  in  dropDuplicates denseSuffixArray

{-|
  Given an array of pairs with prefixes and a suffix for each one,
  and a given prefix, returns an array of suffixes that correspond to such prefix. 
-}
findAllSuffixes :: [([String], String)] -> [String] -> [String]
findAllSuffixes arr prefix =
  [b | (a, b) <- arr, a == prefix]

{-|
  Normalizes a given word with the following criteria:
  * The word must begin with an alphanumeric character
  * The word must end with an alphanumeric character
  * The word must be all in lowercase
-}
normalize :: String -> String
normalize =
  let lowerize = map toLower
      removeStartSymbols = dropWhile (not . isAlpha)
      removeEndSymbols = reverse . removeStartSymbols . reverse
      removeSymbols = removeStartSymbols . removeEndSymbols
  in  lowerize . removeSymbols

{-|
  Given a phrase as a string, separates all the tokens into single,
  normalized words, and returns them in an array.
-}
splitToArray :: String -> [String]
splitToArray [] = []
splitToArray phrase =
  let wordStart = dropWhile (not . isAlpha) phrase
      (word, remainingPhrase) = span isAlpha wordStart
  in  (normalize word) : (splitToArray remainingPhrase)
