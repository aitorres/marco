{- |
Module      :   Main
Description :   Front-end for the Markov Chain based text generator
Copyright   :   (c) Andrés Ignacio Torres
License     :   MIT
Maintainer  :   andresitorresm@gmail.com
Stability   :   experimental

Basic front-end client for the Markov Chain based text generator. Receives
a filename, a context length and an approximate max length for the to-be-generated string,
and echoes such new phrase through the standard output.
-}

module Main (main) where

import Data.Time.Clock.POSIX

import Markov

import System.Environment
import System.Exit
import System.Random
import System.IO

{-|
  Returns a random element out of an array.
-}
getRandomElement :: [a] -> IO (a)
getRandomElement x = do
  i <- randomRIO (0, length x - 1)
  return (x !! i)

{-|
  Procedurally generates a new phrase, given base string encoded
  as a Phrase, an initial prefix and an approx. max length
-}
getNewPhrase :: Phrase -> [String] -> String -> Int -> IO (String)
getNewPhrase phrase first_prefix str max = do
  putStrLn $ "so far, you've got " ++ show (length str) ++ " words"
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

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ progName ++ " usage: " ++ progName ++ " <filename> <context length> <max length>"

checkArgs :: [a] -> IO ()
checkArgs [_, _, _] = return ()
checkArgs _ = do
  printHelp
  exitFailure

{-|
  Main client for the application.
-}
main :: IO ()
main = do
  args <- getArgs
  checkArgs args
  let filename = args !! 0
  let raw_n = args !! 1
  let raw_max = args !! 2
  inputPhrase <- readFile filename
  let n = read raw_n :: Int
  let max = read raw_max :: Int
  let phrase = stringToPhrase inputPhrase
  let prefixes = getPhrasePrefixes phrase n
  first_prefix <- getRandomElement prefixes
  putStrLn $ "Phrase generation: ENGAGED"
  start_time <- getPOSIXTime
  nphrase <- getNewPhrase phrase first_prefix [] max
  finish_time <- getPOSIXTime
  putStrLn $ "Phrase generation: COMPLETED"
  putStrLn $ "Time spent: " ++ (show (finish_time - start_time)) ++ "ms"
  putStrLn $ "Your phrase is: " ++ nphrase
