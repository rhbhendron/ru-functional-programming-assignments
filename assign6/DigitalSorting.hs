module DigitalSorting where

import Data.List
import Data.Char

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

--genericRank :: (Ord key) => [(key,a)] -> [[a]]

--instance Rankable Int where ... etc.

--instance Rankable Bool where ...

--instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where

--etc.

----------------------------------------------------------------------------------------------------
-- some test inputs (it would be reasonably for "rank" and "genericRank" to produce the same output)

testPhrase :: [Char]
testPhrase = "Hello, world!"

boolTest :: [(Bool,Char)]
boolTest = [ (isLetter c, c) | c <- testPhrase ]

maybeTest :: [(Maybe Char,Char)]
maybeTest = [ (if isLetter c then Just c else Nothing, c) | c <- testPhrase ]

tupleTest :: [((Bool,Char),Char)]
tupleTest = [ ((isLetter c, c), c) | c <- testPhrase ]

listTest :: [(String,Char)]
listTest = [ (w, c) | w <- groupBy (\x y->isLetter x==isLetter y) testPhrase, c <- w ]
