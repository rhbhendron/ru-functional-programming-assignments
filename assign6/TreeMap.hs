module TreeMap where

-- this imports the solution to 4.3, which also defines the data type Tree a:
-- data Tree a = Leaf | Node a (Tree a) (Tree a)
import Tree

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree v
  fmap _ Leaf = Leaf
  fmap f (Node x lt rt) = Node (f x) (fmap lt) (fmap lt)
