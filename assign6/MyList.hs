module MyList where

data MyList a = a :# MyList a | Null
  deriving (Eq,Ord,Show)

toList :: MyList a -> [a]
toList Null = []
toList (x :# xs) = x : toList xs

--fromList :: [a] -> MyList a
--fromList [] = ...
--fromList (x : xs) = ...
