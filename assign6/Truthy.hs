module Truthy where

class Truthy a where
  truthy :: a -> Bool

instance Truthy Bool where
  truthy = id

instance Truthy Integer where
  truthy 0 = False
  truthy _ = True

data Nope = Nope

instance Truthy Nope where
  truthy Nope = False

instance (Truthy a, Truthy b) => Truthy (a,b) where
   truthy (a,b) = truthy a && truthy b

infixr 3 &&&
infixr 2 |||

(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
(&&&) x y = truthy (x,y)

(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
(|||) x y = truthy x || truthy y

ifThenElse :: (Truthy a) => a -> b -> b -> b
ifThenElse x y z = if truthy x then y else z
