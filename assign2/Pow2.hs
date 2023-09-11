module Pow2 where

--2.3.1 and 2.3.2
pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)

--2.3.3
{-
There is no fixed upper bound for Integer values in Haskell. In this case, n is limited by system memory.
ghci> pow2 9999999 :: Integer
*** Exception: stack overflow

Max Int: n = 62
ghci> maxBound :: Int
9223372036854775807
ghci> pow2 62 :: Int
4611686018427387904
ghci> pow2 63 :: Int
-9223372036854775808

Max Float: n = 127
ghci> pow2 127 :: Float
1.7014118e38
ghci> pow2 128 :: Float
Infinity

Max Double: n = 1023
ghci> pow2 1023 :: Double
8.98846567431158e307
ghci> pow2 1024 :: Double
Infinity
-}



