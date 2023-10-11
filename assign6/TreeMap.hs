module TreeMap where

-- this imports the solution to 4.3, which also defines the data type Tree a:
-- data Tree a = Leaf | Node a (Tree a) (Tree a)
import Tree

instance Functor Tree where
  -- fmap :: ???
  fmap _ Leaf = ...
  fmap f (Node x lt rt) = ...
