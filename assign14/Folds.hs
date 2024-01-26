module Folds where

import Data.List
import Data.Function
import Data.Foldable

mySum :: (Foldable f, Num a) => f a -> a
mySum xs = error "TODO: implement me"

myLength :: (Foldable f) => f a -> Int
myLength xs = error "TODO: implement me"

slowMean :: (Foldable t) => t Integer -> Double
slowMean xs = fromIntegral (mySum xs) / fromIntegral (myLength xs)

myMean :: (Foldable t) => t Integer -> Double
myMean xs = error "TODO: implement me"

bigList :: [Integer]
bigList = take 10000000 $ unfoldr (Just . (\x->(x `mod` 10+1,(75*(x+1)-1) `mod` 0x10001))) 37
