module Lego where

import Data.List
import Data.Tuple

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs

tag :: [a] -> [(a,Int)]
tag xs = snd $ mapAccumL (\x y -> (x + 1, (y, x))) 0 xs

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sortOn fst $ tag xs

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos a = map unwrap $ (sortOn (snd . fst) (tag (sortWithPos a)))
    where unwrap ((a,b),c) = (a,c)
