module Main where

import System.Random
import System.IO
import Control.Monad.State
import Markov

getRandomElement :: Show a => [a] -> IO (a)
getRandomElement x = do
  i <- randomRIO (0, length x - 1)
  return (x !! i)

getNewPhrase :: Phrase -> [String] -> String -> Int -> IO (String)
getNewPhrase phrase first_prefix str max = do
  let suffixes = getPrefixSuffixes phrase first_prefix
  case length suffixes of
    0 -> do
      return str
    otherwise -> do
      case max of
        0 ->
          return str
        otherwise -> do
          suffix <- getRandomElement suffixes
          let new_prefix = (tail first_prefix) ++ [suffix]
          getNewPhrase phrase new_prefix (str ++ " " ++ suffix) (max - 1)

main :: IO ()
main = do
  putStr "Phrase: "
  hFlush stdout
  input <- getLine
  putStr "Context length: "
  hFlush stdout
  raw_n <- getLine
  let n = read raw_n :: Int
  putStr "Max length: "
  hFlush stdout
  raw_max = getLine
  let max = read raw_max :: Int
  let phrase = stringToPhrase input
  let prefixes = getPhrasePrefixes phrase n
  first_prefix <- getRandomElement prefixes
  nphrase <- getNewPhrase phrase first_prefix [] max
  putStrLn nphrase

