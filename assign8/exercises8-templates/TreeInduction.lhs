> module TreeInduction where
> 
> data Tree a = Leaf | Node a (Tree a) (Tree a)
>   deriving (Show)

1: What is the induction scheme for trees?

Local definitions:

> leaves :: Tree a -> Int
> leaves Leaf = 1
> leaves (Node _ l r) = leaves l + leaves r
>
> nodes :: Tree a -> Int
> nodes Leaf = 0
> nodes (Node _ l r) = 1 + nodes l + nodes r

2: To prove: leaves t = nodes t + 1
By induction on t.

Case ...
