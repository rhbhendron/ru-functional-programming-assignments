module FindMinimum where

import Data.List

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let e = minimum xs in e : selectionSort (delete e xs)

leastElem :: (Ord a) => [a] -> a
leastElem = head . selectionSort

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
{- note: insert is in the Prelude, defined as:
  insert a [] = [a]
  insert a (b:xs)
    | a <= b    = a : b : xs
    | otherwise = b : insert a xs
-}

leastElem' :: (Ord a) => [a] -> a
leastElem' = head . insertionSort

someNums :: [Int]
someNums = [ 257*x `mod` 1337 | x <- [1..100000] ]
