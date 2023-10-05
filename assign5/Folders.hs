module Folders where

import Prelude hiding (and,or,elem,maximum)

--and :: [Bool] -> Bool

--or :: [Bool] -> Bool

--elem :: (Eq a) => a -> [a] -> Bool

--maximum :: (Ord a) => [a] -> a

--fromList :: (Ord a) => [a] -> Tree a

--fromBits :: [Integer] -> Integer

{- -------------------------------------------------------------------}

-- the relevant definitions for 'fromList'

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x tree@(Node key lt rt)
  | x < key   = Node key (insert x lt) rt
  | x > key   = Node key lt (insert x rt)
  | otherwise = tree
