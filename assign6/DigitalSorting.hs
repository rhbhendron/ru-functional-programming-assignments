module DigitalSorting where

import Data.List
import Data.Char
-- import Deck

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank =  map (map snd) . groupBy (\x y -> fst x == fst y) . sortOn fst

instance Rankable Int where
  rank :: [(Int,a)] -> [[a]]
  rank = genericRank

instance Rankable Char where
  rank :: [(Char, a)] -> [[a]]
  rank = genericRank

instance Rankable Integer where
  rank :: [(Integer, a)] -> [[a]]
  rank = genericRank

instance Rankable Bool where
  rank :: [(Bool,a)] -> [[a]]
  rank = genericRank

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
