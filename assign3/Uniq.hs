module Uniq where

import Data.List

--3.3.5
uniq :: (Eq a) => [a] -> [a]
uniq xs = map head $ group xs

{-
Another definition could be

uniq [] = []
uniq [x] = [x]
uniq (x:y:xs) = if x == y then x:(uniq xs) else x : uniq (y:xs)

-}
