module Notation where

import Data.Time

siri :: IO ()
siri = 
  putStrLn "What is your name?" >>
  getLine >>= \name ->
  getZonedTime >>= \now ->
  putStrLn (name ++ formatTime defaultTimeLocale ", the time is %H:%M" now)

mayLookup :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
mayLookup maybekey assocs = do
  key <- maybekey
  result <- lookup key assocs
  return result
