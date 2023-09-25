module MyList where

data MyList a = a :# MyList a | Null
  deriving (Eq,Ord,Show)
