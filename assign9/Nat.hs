module Nat where

data Nat = O | S Nat
  deriving Show

--instance Eq Nat where
--instance Ord Nat where

toNat :: Integer -> Nat
toNat n | n < 0 = error "can't convert negative values to Nat"
toNat 0 = O
toNat n = S (toNat (n-1))

-- "canned recursion" on Nat's
natFoldr :: (a -> a) -> a -> Nat -> a
natFoldr f b = go
  where
  go O     = b
  go (S m) = f (go m)

fromNat :: (Integral n) => Nat -> n
fromNat = natFoldr (+1) 0

infinity :: Nat
infinity = S infinity

--(+.) :: Nat -> Nat -> Nat
--(-.) :: Nat -> Nat -> Nat

--natLength :: [a] -> Nat
--
