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
getNewPhrase :: [MarkovToken] -> Phrase -> [String] -> String -> Int -> IO (String)
getNewPhrase toks phrase first_prefix str max = do
  putStrLn $ "so far, you've got: " ++ str
  let suffixes = getPrefixSuffixes' toks first_prefix
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
          getNewPhrase toks phrase new_prefix (str ++ " " ++ suffix) (max - 1)

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ progName ++ " usage: " ++ progName ++ " <filename> <context length> <max length>"

checkArgs :: [a] -> IO ()
checkArgs [_, _, _] = return ()
checkArgs _ = do
  printHelp
  exitFailure

properPrintLn :: String -> IO ()
properPrintLn str = do
  putStrLn str
  hFlush stdout

{-|
  Main client for the application.
-}
main :: IO ()
main = do
  -- Validates arguments (or exits with a help message if they are invalid)
  args <- getArgs
  checkArgs args

  -- Parses arguments
  let filename = args !! 0
  let raw_n = args !! 1
  let raw_max = args !! 2
  let n = read raw_n :: Int
  let max = read raw_max :: Int

  -- Reads filename
  file_read_start_time <- getPOSIXTime
  inputPhrase <- readFile filename
  file_read_finish_time <- getPOSIXTime
  properPrintLn $ "File properly loaded into memory after " ++ (show (file_read_finish_time - file_read_start_time))  ++ "\n"

  -- Pre-processes the data
  properPrintLn $ "Reading file's content and pre-processing into proper data structures"
  pre_proc_start_time <- getPOSIXTime
  let phrase = stringToPhrase inputPhrase
  let tokens = getPhraseTokens phrase n
  let prefixes = getPhrasePrefixes' tokens
  first_prefix <- getRandomElement prefixes
  pre_proc_finish_time <- getPOSIXTime
  properPrintLn $ "Time spent (pre-processing): " ++ (show (pre_proc_finish_time - pre_proc_start_time)) ++ "\n"

  -- Generates the new text
  properPrintLn $ "Phrase generation: ENGAGED"
  proc_start_time <- getPOSIXTime
  nphrase <- getNewPhrase tokens phrase first_prefix [] max
  proc_finish_time <- getPOSIXTime
  properPrintLn $ "Phrase generation: COMPLETED"
  properPrintLn $ "Time spent (generation): " ++ (show (proc_finish_time - proc_start_time)) ++ "\n"

  -- Reports the text and exits
  properPrintLn $ "Your phrase is: " ++ nphrase
