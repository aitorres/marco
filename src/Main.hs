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

import           Control.Monad         (when)

import           Data.List.Split
import           Data.Time.Clock.POSIX

import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Random

import           Markov

{-|
  Returns a random element out of an array.
-}
getRandomElement :: [a] -> IO a
getRandomElement x = do
  i <- randomRIO (0, length x - 1)
  return (x !! i)

{-|
  Procedurally generates a new phrase, given base string encoded
  as a Phrase, an initial prefix and an approx. max length
-}
getNewPhrase :: [MarkovToken] -> Phrase -> [String] -> String -> Int -> IO String
getNewPhrase toks phrase first_prefix str max = do
  let suffixes = getPrefixSuffixes' toks first_prefix
  if null suffixes || max == 0 then return str
  else do
    suffix <- getRandomElement suffixes
    let new_prefix = tail first_prefix ++ [suffix]
    getNewPhrase toks phrase new_prefix (str ++ " " ++ suffix) (max - 1)

{-|
  Prints a small help message, containing the proper usage of the application
-}
printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn $ progName ++ " usage (training): \t"   ++ progName ++ " train    <folder name> <context length>"
  putStrLn $ progName ++ " usage (generation): \t" ++ progName ++ " generate <max length>"

{-|
  Performs a minor check of the arguments received from command line. If invalid,
  shows the help message and quits.
-}
checkArgs :: [String] -> IO String
checkArgs args = do
  let trainSyntax = (length args == 3) && (head args == "train")
  let genSyntax = (length args == 2) && (head args == "generate")

  if trainSyntax || genSyntax then return (head args)
  else do
    printHelp
    exitFailure

{-|
  Prints a given string and immediately flushes the standard output, in order to ensure
  I/O delays are kept to a minimum.
-}
properPrintLn :: String -> IO ()
properPrintLn str = do
  putStrLn str
  hFlush stdout

{-|
  Joins the content of several training data files
  into one string to be parsed afterwards.
-}
foldTrainingData :: FilePath -> [FilePath] -> IO String
foldTrainingData directory files =
  let targetFile = (directory ++ "/" ++ head files) in
    if null files then return ""
    else if length files == 1 then readFile targetFile
    else do
      file <- readFile targetFile
      remainingFiles <- foldTrainingData directory (tail files)
      return (file ++ remainingFiles)

{-|
  Trains the markov generator with the contents of a given folder.
  Assumes all the files are simple txt files.
-}
train :: FilePath -> Int -> IO ()
train directory n = do
  properPrintLn "Reading data files content and training into proper data structures"
  pre_proc_start_time <- getPOSIXTime

  foundFiles <- getDirectoryContents directory
  let files = [x | x <- foundFiles, "txt" == last (splitOn "." x) ]

  properPrintLn "Training files loaded into memory"
  trainingData <- foldTrainingData directory files

  properPrintLn "Training files parsed together. Training in process..."
  let phrase = stringToPhrase trainingData
  let tokens = getPhraseTokens phrase n
  let prefixes = getPhrasePrefixes' tokens

  -- Writes obtained data into disk
  createDirectoryIfMissing True "trained-data"
  writeFile "trained-data/phrase.txt" (show phrase)
  writeFile "trained-data/tokens.txt" (show tokens)
  writeFile "trained-data/prefixes.txt" (show prefixes)

  pre_proc_finish_time <- getPOSIXTime
  properPrintLn $ "Time spent (training): " ++ show (pre_proc_finish_time - pre_proc_start_time) ++ "\n"

{-|
  Checks if the required training files exist, and stops execution if they
  don't, printing a helpful message to standard output.
-}
checkTrainingFiles :: IO ()
checkTrainingFiles = do
  checkTokens <- doesFileExist "trained-data/tokens.txt"
  checkPhrase <- doesFileExist "trained-data/phrase.txt"
  checkPrefixes <- doesFileExist "trained-data/prefixes.txt"

  let checks = [checkTokens, checkPhrase, checkPrefixes]
  when (False `elem` checks) $ do
    properPrintLn "Training data not found. Check help and train marco first."
    exitFailure

{-|
  Main client for the application.
-}
main :: IO ()
main = do
  -- Validates arguments (or exits with a help message if they are invalid)
  args <- getArgs
  execMode <- checkArgs args

  case execMode of
    "train" -> do
      -- Parses arguments
      let foldername = args !! 1
      let raw_n = args !! 2
      let n = read raw_n :: Int

      -- Trains marco according to the given training dataset
      train foldername n
    "generate" -> do
      -- Parses arguments
      let raw_max = args !! 1
      let max = read raw_max :: Int

      -- Starts the new text generation
      properPrintLn "Phrase generation: ENGAGED"

      -- If the training files exist, reads and parses them
      checkTrainingFiles

      phraseStr <- readFile "trained-data/phrase.txt"
      tokensStr <- readFile "trained-data/tokens.txt"
      prefixesStr <- readFile "trained-data/prefixes.txt"

      let phrase = read phraseStr :: Phrase
      let tokens = read tokensStr :: [MarkovToken]
      let prefixes = read prefixesStr :: [[String]]

      -- Starts text generation
      proc_start_time <- getPOSIXTime
      first_prefix <- getRandomElement prefixes
      nphrase <- getNewPhrase tokens phrase first_prefix [] max
      proc_finish_time <- getPOSIXTime
      properPrintLn "Phrase generation: COMPLETED"
      properPrintLn $ "Time spent (generation): " ++ show (proc_finish_time - proc_start_time) ++ "\n"

      -- Reports the text and exits
      properPrintLn $ "Your phrase is: " ++ nphrase
