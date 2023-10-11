module DigitalSorting where

import Data.List
import Data.Char
import Data.Maybe

class Rankable key where
  rank :: [(key,a)] -> [[a]]

digitalSortOn :: (Rankable key) => (v -> key) -> [v] -> [v]
digitalSortOn f = concat . rank . map (\x->(f x, x))

digitalSort :: (Rankable key) => [key] -> [key]
digitalSort = digitalSortOn id

genericRank :: (Ord key) => [(key,a)] -> [[a]]
genericRank = map (map snd). groupBy (\x y -> fst x == fst y) . sortOn fst

instance Rankable Int where
  rank = genericRank

instance Rankable Integer where
  rank = genericRank

instance Rankable Char where
  rank = genericRank

instance Rankable Bool where
  rank = foldr bucket [[],[]] where
    bucket (f,s) ([xs,ys]) = if f then [xs,(s:ys)] else [(s:xs),ys]

assoc :: ((k1,k2),a) -> (k1, (k2,a))
assoc ((x1,x2),x3) = (x1,(x2,x3))

instance (Rankable key1, Rankable key2) => Rankable (key1,key2) where
  rank :: (Rankable key1, Rankable key2) => [((key1,key2),a)] -> [[a]]
  rank = concatMap rank . rank . map assoc

bucketMaybes :: [(Maybe a, b)] -> [[(Maybe a, b)]]
bucketMaybes = foldr bucket [[],[]] where
  bucket x ([xs,ys]) = case fst x of
                         Just y -> [xs,(x:ys)]
                         Nothing -> [(x:xs),ys]

instance (Rankable key , Ord key) => Rankable (Maybe key) where
  rank xs = let
    justs = map (\(x,y) -> (fromJust x, y)) (filter (isJust . fst) xs)
    nones = map snd (filter (isNothing . fst) xs)
    in nones : rank justs

-- instance (Rankable key) => Rankable [key] where
--   rank :: (Rankable key) => [([key], a)] -> [[a]]

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
