module Expression where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show)

skewed :: Integer -> Tree ()
skewed 0 = Leaf
skewed n = Node () (skewed (n-1)) Leaf

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node x lt rt) = inorder lt ++ [x] ++ inorder rt

{-
Derive
  inorderCat t xs = inorder t ++ xs

  inorderCat Leaf xs = inorder Leaf ++ xs
  inorderCat Leaf xs = [] ++ xs
  inorderCat Leaf xs = xs

  inorderCat l xs = inorder l ++ xs
  inorderCat r xs = inorder r ++ xs

  inorderCat (Node a l r) xs = inorder (Node a l r) ++ xs
  inorderCat (Node a l r) xs = (inorder l ++ [a] ++ inorder r) ++ xs
  inorderCat (Node a l r) xs = inorder l ++ ([a] ++ inorder r ++ xs)
  inorderCat (Node a l r) xs = inorderCat l ([a] ++ inorder r ++ xs)
  inorderCat (Node a l r) xs = inorderCat l ([a] ++ inorderCat r xs)
  inorderCat (Node a l r) xs = inorderCat l (a : inorderCat r xs)
-}

inorderCat :: Tree a -> [a] -> [a]
inorderCat Leaf xs = xs
inorderCat (Node x l r) xs = inorderCat l (x : inorderCat r xs)

inorder' :: Tree a -> [a]
inorder' t = inorderCat t []

-- TODO: make me more efficient, too
elems :: Tree a -> [a]
elems Leaf = []
elems (Node x lt rt) = x : elems lt ++ elems rt

elemsCat :: Tree a -> [a] ->  [a]
elemsCat Leaf xs = xs
elemsCat (Node a l r) xs = a : (elemsCat l (elemsCat r xs))

elems' :: Tree a -> [a]
elems' t = elemsCat t []
