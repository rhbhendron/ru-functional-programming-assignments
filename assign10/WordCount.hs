-- ghc --make WordCount.hs
module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  fileContents <- mapM readFile args
  let wcs = map wcCount fileContents
  let count_file = zip wcs args
  mapM_ print count_file

wcCount :: String -> [Int]
wcCount str = [length (lines str), length (words str), length str]
