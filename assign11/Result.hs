{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap f (Okay a) = Okay (f a)
  fmap f (Error xs) = Error xs


instance Applicative Result where
  pure :: a -> Result a
  pure = Okay

  (<*>) :: Result (a -> b) -> Result a -> Result b
  (Okay f) <*> x = fmap f x
  (Error xs) <*> (Error ys) = Error (xs++ys)
  (Error xs) <*> _ = Error xs
  _ <*> (Error ys) = Error ys
