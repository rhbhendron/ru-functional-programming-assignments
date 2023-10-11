module Nat where

data Nat = O | S Nat
  deriving (Show, Eq, Ord)

fromNat :: (Num t) => Nat -> t
fromNat O     = 0
fromNat (S x) = 1 + fromNat x

toNat :: (Ord t, Num t) => t -> Nat
toNat n
    | n < 0     = error "toNat only works for non-negative numbers"
    | n == 0    = O
    | otherwise = S (toNat (n - 1))

--instance Eq Nat where 
--  ...

--instance Ord Nat where 
--  ...

instance Enum Nat where
  succ x     = error "TODO"
  pred x     = error "TODO"
  toEnum x   = error "TODO"
  fromEnum x = error "TODO"
