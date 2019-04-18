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
  stringToPhrase,
  getPhraseTokens,
  getPhrasePrefixes,
  getPrefixSuffixes
) where

import Data.Char
import Data.List.Split

data Phrase =
  Phrase [String]
  deriving (Eq, Show)

data DisjointMarkovToken =
  DisjointMarkovToken ([String], String)
  deriving (Eq, Show)

data MarkovToken =
  MarkovToken ([String], [String])
  deriving (Eq, Show)

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

{-|
  Given a phrase as a string, returns a proper Phrase instance.
-}
stringToPhrase :: String -> Phrase
stringToPhrase x = Phrase (splitToArray x)

{-|
  Given a phrase, returns an array of MarkovTokens with all the prefixes
  and all of their suffixes
-}
getPhraseTokens :: Phrase -> [MarkovToken]
getPhraseTokens x = compactSuffixArray $ getSuffixArray x 2

{-|
  Given a phrase, returns an array with arrays of strings, each with the possible prefixes.
-}
getPhrasePrefixes :: Phrase -> [[String]]
getPhrasePrefixes x = [y | (MarkovToken (y, _)) <- getPhraseTokens x]

{-|
  Given a phrase and a prefix, returns such prefix suffixes if they exist.
-}
getPrefixSuffixes :: Phrase -> [String] -> [String]
getPrefixSuffixes phrase prefix = [x | y <- [b | MarkovToken (a, b) <- getPhraseTokens phrase, a == prefix], x <- y]

{-|
  Given an array of strings (words) and a context length,
  returns an array with each possible prefix (as an array) and the
  corresponding suffix.
-}
getSuffixArray :: Phrase -> Int -> [DisjointMarkovToken]
getSuffixArray (Phrase []) _ = []
getSuffixArray (Phrase words) n
  | length words > n =
    let prefix = take n words
        suffix = words !! n
        remainingWords = tail words
    in  (DisjointMarkovToken (prefix, suffix)) : getSuffixArray (Phrase remainingWords) n
  | otherwise = []

{-|
  Given a suffix array, "compresses" the array in order to find common prefixes
  and associate all of the suffixes to them.
-}
compactSuffixArray :: [DisjointMarkovToken] -> [MarkovToken]
compactSuffixArray arr =
  let denseSuffixArray = [(MarkovToken (prefix, findAllSuffixes arr prefix)) | (DisjointMarkovToken (prefix, _)) <- arr]
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
findAllSuffixes (arr) prefix =
  [b | (DisjointMarkovToken (a, b)) <- arr, a == prefix]
