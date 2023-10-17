module MyList where

data MyList a = a :# MyList a | Null
  deriving (Eq,Ord)

toList :: MyList a -> [a]
toList Null = []
toList (x :# xs) = x : toList xs

fromList :: [a] -> MyList a
fromList = foldr (:#) Null

x :: [Int]
x = [1..5]

y :: [Int]
y = [1..6]

test :: Bool
test = not (fromList x <= fromList y) == (x <= y)

{-
        fromList x is not <= fromList y, but x <= y. I think this is because, the derived Ord
        only compares things lexicographically, and does not consider the length of the, so if 2 lists
        one of size n, and another of size m, where n < m, and the first n elements of the lists of length m
        is the same as list n, then they will be equal to each other. I think this may be due to the fact that
        List is defined as Null | Cons a List a, where as MyList has the cons first, which I think will lead to
        when 1 list reaches the form Null, if it is of data type List, then the Ord typeclass will think it is less
        but not if the definition is the other way around
-}

instance (Show a) => Show (MyList a) where
  show xs = "fromList " ++ show (toList xs)
