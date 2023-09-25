module Concat where

concatr :: [[a]] -> [a]
concatr [] = []
concatr (xs:xss) = xs ++ concat xss

concatl :: [[a]] -> [a]
concatl [] = []
concatl (xs:xss) = concat xss ++ xs
