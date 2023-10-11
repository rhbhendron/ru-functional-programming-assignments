module Nat where

data Nat = O | S Nat
  deriving (Show)

fromNat :: (Num t) => Nat -> t
fromNat O     = 0
fromNat (S x) = 1 + fromNat x

toNat :: (Ord t, Num t) => t -> Nat
toNat x
  | x <= 0 = O
  | otherwise = S (toNat (x - 1))

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) O O = True
  (==) O (S x) = False
  (==) (S x) O = False
  (==) (S x) (S y) = (==) x y

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (S x) <= O = False
  O <= (S x) = True
  (S x) <= (S y) = (x == y) || (x <= y)

instance Enum Nat where
  succ      = S
  pred      = toNat . flip (-) 1 . fromNat
  toEnum    = toNat
  fromEnum  = fromNat
