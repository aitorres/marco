{- |
Module      :   Markov
Description :   Basic implementation of a Markov Chain algorithm for text
Copyright   :   (c) Andrés Ignacio Torres
License     :   MIT
Maintainer  :   andresitorresm@gmail.com
Stability   :   experimental

Basic implementation of an arbitrary-length Markov Chain algorithm.
-}

module Markov (
  Phrase,
  MarkovToken,
  stringToPhrase,
  getPhraseTokens,
  getPhrasePrefixes',
  getPrefixSuffixes'
) where

import           Data.Char
import           Data.List.Split

{-|
  Custom type to save a phrase as an array of separate,
  possibly normalized words. Implements a concrete instance of Show
  in order to print the phrase as faithfully as possible.
-}
type Phrase = [String]

{-|
  Custom type to save a pair of an arbitrary prefix
  and one associated suffix.
-}
type DisjointMarkovToken = ([String], String)

{-|
  Custom type to save a pair of an arbitrary prefix
  and an array of associated suffixes. Intended to be able to
  be formed as a "join" / "fold" of several DisjointMarkovToken instances.
-}
type MarkovToken = ([String], [String])

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
  in  lowerize . removeEndSymbols

{-|
  Given a phrase as a string, separates all the tokens into single,
  normalized words, and returns them in an array.
-}
splitToArray :: String -> [String]
splitToArray [] = []
splitToArray phrase =
  let wordStart = dropWhile (not . isAlpha) phrase
      (word, remainingPhrase) = span isAlpha wordStart
  in  normalize word : splitToArray remainingPhrase

{-|
  Given a phrase as a string, returns a proper Phrase instance.
-}
stringToPhrase :: String -> Phrase
stringToPhrase = splitToArray

{-|
  Given a phrase, returns an array of MarkovTokens with all the prefixes
  and all of their suffixes
-}
getPhraseTokens :: Phrase -> Int -> [MarkovToken]
getPhraseTokens p n = compactSuffixArray $ getSuffixArray p n

{-|
  Given a phrase, returns an array with arrays of strings, each with the possible prefixes.
-}
getPhrasePrefixes :: Phrase -> Int -> [[String]]
getPhrasePrefixes p n =
  [y | (y, _) <- getPhraseTokens p n]

{-|
  Alternative version of getPhrasePrefixes: given an array of MarkovTokens, returns an array
  with arrays of strings, each with the possible prefixes.
-}
getPhrasePrefixes' :: [MarkovToken] -> [[String]]
getPhrasePrefixes' toks =
  [y | (y, _) <- toks]

{-|
  Given a phrase and a prefix, returns such prefix suffixes if they exist.
-}
getPrefixSuffixes :: Phrase -> [String] -> [String]
getPrefixSuffixes phrase prefix =
  [x | y <- [b | (a, b) <- getPhraseTokens phrase (length prefix), a == prefix], x <- y]

{-|
  Alternative version of getPrefixSuffixes: given an array of MarkovTokens, and a
  prefix, returns such prefix suffixes if they exist.
-}
getPrefixSuffixes' :: [MarkovToken] -> [String] -> [String]
getPrefixSuffixes' toks prefix =
  [x | y <- [b | (a, b) <- toks, a == prefix], x <- y]

{-|
  Given an array of strings (words) and a context length,
  returns an array with each possible prefix (as an array) and the
  corresponding suffix.
-}
getSuffixArray :: Phrase -> Int -> [DisjointMarkovToken]
getSuffixArray [] _ = []
getSuffixArray words n
  | length words > n =
    let prefix = take n words
        suffix = words !! n
        remainingWords = tail words
    in  (prefix, suffix) : getSuffixArray remainingWords n
  | otherwise = []

{-|
  Given a suffix array, "compresses" the array in order to find common prefixes
  and associate all of the suffixes to them.
-}
compactSuffixArray :: [DisjointMarkovToken] -> [MarkovToken]
compactSuffixArray arr =
  let denseSuffixArray = [ (prefix, findAllSuffixes arr prefix) | (prefix, _) <- arr]
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
findAllSuffixes :: [DisjointMarkovToken] -> [String] -> [String]
findAllSuffixes arr prefix =
  [b | (a, b) <- arr, a == prefix]
