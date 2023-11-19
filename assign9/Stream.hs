module Stream where

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (16::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

head :: Stream a -> a
head (a :> as) = a

tail :: Stream a -> Stream a
tail (a :> as) = as

repeat :: a -> Stream a
repeat a = a :> repeat a

map :: (a -> b) -> (Stream a -> Stream b)
map f (a :> as) = f a :> map f as

zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zipWith f (a :> as) (b :> bs) = f a b :> zipWith f as bs

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (a :> as) = if f a then a :> filter f as else filter f as

{-
  When filter (\x -> False) (from 0) is called, for each element
  of (from 0), we see if (\x -> False) a is True. Since it is never True,
  we iterate through the Stream. Since the Stream is an infinite data type,
  the iteration will never stop, and since no Stream is being returned, nothing
  will be displayed to the string, so the program will loop infinitely.
-}

toList :: Stream a -> [a]
toList (a :> as) = a : toList as

cycle :: [a] -> Stream a
cycle xs = cyc xs where
  cyc [] = cyc xs
  cyc (a:as) = a :> cyc as

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

isPrime :: Integer -> Bool
isPrime x = null [k | k <- [2 .. x-1] , x `mod` k == 0]

primes :: Stream Integer
primes = filter isPrime (from 2)

primetwins :: Stream (Integer,Integer)
primetwins = (filter (\x -> x /= (0,0)) . zipWith areTwins (1 :> primes)) primes where
  areTwins x y = if x + 2 == y then (x,y) else (0,0)

combine :: Stream a -> Stream a -> Stream a
combine (a :> as) (b :> bs) = a :> b :> combine as bs
