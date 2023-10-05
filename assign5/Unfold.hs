module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))

-- define all the below funtions using `unfoldr`
bits :: Int -> [Int]
bits n = unfoldr divi n where
  divi 0 = Nothing
  divi n = Just(n `mod` 2, n `div` 2)

zip :: [a] -> [b] -> [(a,b)]
zip xs ys = unfoldr zippy (xs,ys) where
  zippy ([],_) = Nothing
  zippy (_,[]) = Nothing
  zippy ((x:xs),(y:ys)) = Just((x,y), (xs,ys))

take :: Int -> [a] -> [a]
take n xs = unfoldr takey (n,xs) where
  takey (_,[]) = Nothing
  takey (0,_) = Nothing
  takey (n, (x:xs)) = Just(x,(n-1,xs))

primes :: [Integer]
primes = unfoldr primey [2..] where
  primey [] = Nothing
  primey (x:xs) = Just(x,[n | n <- xs, n `mod` x /= 0])
-- alternative implementation of `primes`:
primes' = sieve [2..]
  where sieve (p:xs) = p : sieve [ n | n <- xs, n `mod` p /= 0 ]

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

(++) :: [a] -> [a] -> [a]
(++) xs ys = apo app (xs,ys) where
  app ([],ys) = Left ys
  app ((x:xs),ys) = Right(x,(xs,ys))

insert :: (Ord a) => a -> [a] -> [a]
insert x ys = apo ins (x,ys) where
  ins (x,[]) = Left [x]
  ins (x, (y:ys))
    | x <= y = Left (x:y:ys)
    | otherwise = Right(y,(x,ys))
unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
unfoldrApo f x = apo ufold (f,x) where
  ufold (f,x) = case f x of
                   Just(a, new) -> Right(a,(f,new))
                   Nothing -> Left []
