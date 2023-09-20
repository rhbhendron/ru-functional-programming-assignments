module Reverse where

import Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev []     acc = acc
        rev (y:ys) acc = rev ys (y:acc)

