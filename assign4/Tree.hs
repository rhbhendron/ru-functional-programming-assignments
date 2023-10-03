module Tree where

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

{----------- exercise 4.3 -------------}

tree :: Tree Char
tree = Node 'c' (Node 'a' Leaf (Node 'b' Leaf Leaf)) (Node 'f' (Node 'd' Leaf Leaf) (Node 'g' Leaf Leaf))

leaves :: Tree a -> Int
leaves Leaf = 1
leaves (Node a t1 t2) = leaves t1 + leaves t2

nodes  :: Tree a -> Int
nodes Leaf = 0
nodes (Node a t1 t2) = 1 + nodes t1 + nodes t2

height :: Tree a -> Int
height Leaf = 0
height (Node a t1 t2) = 1 + max (height t1) (height t2)

elems  :: Tree a -> [a]
elems Leaf = []
elems (Node a t1 t2) = [a] ++ elems t1 ++ elems t2

isSearchTree :: (Ord a) => Tree a -> Bool
isSearchTree Leaf = True
isSearchTree (Node a Leaf Leaf) = True
isSearchTree (Node a Leaf (Node b t1 t2)) = a < b && isSearchTree t1 && isSearchTree t2
isSearchTree (Node a (Node b t1 t2) Leaf) = a > b && isSearchTree t1 && isSearchTree t2
isSearchTree (Node a (Node b t11 t12) (Node c t21 t22)) = and $ [a>b,a<c] ++ (map isSearchTree [(Node b t11 t12), (Node c t21 t22)])

{----------- exercise 4.4 -------------}

member :: (Ord a) => a -> Tree a -> Bool
member x Leaf = False
member x (Node y t1 t2)
  | x < y = member x t1
  | x > y = member x t2
  | otherwise = True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node a t1 t2)
  | x < a = Node a (insert x t1) t2
  | x > a = Node a t1 (insert x t2)
  | otherwise = Node a t1 t2

delete :: (Ord a) => a -> Tree a -> Tree a
delete x (Node a Leaf Leaf) = if x == a then Leaf else Node a Leaf Leaf
delete x (Node a t1 Leaf) = if x == a then t1 else Node a (delete x t1) Leaf
delete x (Node a Leaf t1) = if x == a then t1 else Node a Leaf (delete x t1)
delete x (Node a t1 t2)
  | x < a = Node a (delete x t1) t2
  | x > a = Node a t1 (delete x t2)
  | otherwise = Node inorderSuccessor (delete inorderSuccessor t1) t2 where
      inorderSuccessor = (maximum . elems) t1
{----------- exercise 4.5 -------------}

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node a Leaf Leaf) = [a]
inOrder (Node a t1 t2) = inOrder t1 ++ [a] ++ inOrder t2


fromAscList :: (Ord a) => [a] -> Tree a
fromAscList [] = Leaf
fromAscList xs = Node n (fromAscList start) (fromAscList end) where
  (start,(n:end)) = splitAt (length xs `div` 2) xs

bfs :: [Tree a] -> [a]
bfs [] = []
bfs (Leaf : xs) = bfs xs
bfs ((Node a t1 t2) : xs) = a : bfs (xs ++ [t1, t2])

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]

{- BONUS: a tree pretty printer; the recursive structure of this function
 - is prety simple, but it is a fiddly function to write if you want it to
 - produce an actually nice tree. -}


layout :: (Show a) => Tree a -> String
layout tree = go "" ("","","") tree
  where
  width = maximum (0:[ length (show e) | e <- elems tree ])
  pad s = let s' = show s in replicate (width-length s') '-' ++ s'
  fill  = replicate width ' '

  --go pre (_,_,preN) Leaf = pre ++ preN ++ "·\n" -- this explicitly draws the leaves
  --go _   _          Leaf = ""                   -- this vertically compresses the tree
  go pre _          Leaf = pre ++ "\n"            -- use more vertical space, but don't draw leaves
  go pre (preR,preL,preN) (Node k lt rt)
    = go (pre ++ preR) (hfill,v_bar,rbend) rt
      ++ (pre ++ preN) ++ pad k ++ junct ++
      go (pre ++ preL) (v_bar,hfill,lbend) lt

  junct = "+\n"         -- change to "+\n" if no Unicode
  hfill = fill ++ "  "
  rbend = fill ++ "/-"  -- change to "/-" if no Unicode
  v_bar = fill ++ "| "  -- change to "| " if no Unicode
  lbend = fill ++ "\\-"  -- change to "\\-" if no Unicode

putTree :: (Show a) => Tree a -> IO()
putTree tree = putStr (layout tree)
