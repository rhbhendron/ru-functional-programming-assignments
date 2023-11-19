module EditDistance where

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1+distance xs (y:ys), 1+distance (x:xs) ys, cost x y+distance xs ys]

  cost x y = if x==y then 0 else 1

editDistance :: String -> String -> Int
editDistance xs ys = distArray ! (0, 0)
  where
    ma = length xs
    mb = length ys

    distArray = array ((0,0), (ma, mb)) [((i,j), distance i j) | i <- [0..ma], j <- [0..mb]]

    distance x y | x == ma = mb - y
                 | y == mb = ma - x
                 | otherwise = minimum [1 + distArray ! (x + 1, y),
                                        1 + distArray ! (x, y + 1),
                                        cost (xs !! x) (ys !! y) + distArray ! (x + 1, y + 1)]
    cost x y | x == y = 0
             | otherwise = 1
