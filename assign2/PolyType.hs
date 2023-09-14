module PolyType where

--2.4.1 f8 can be used on arguments of type String
f8 :: Ord a => a -> a -> a
f8 x y  = if x <= y then x else y

f9 :: Bool -> Bool -> Bool
f9 x y  = not x || y

f10 :: (Eq p, Num p) => p -> p -> p
f10 x y
  | x == 0    = y
  | otherwise = x + y

--2.4.1 f11 can be used on arguments of type String
f11 :: p -> p -> p
f11 x y = get 0
  where get n = if n == 0 then x else y
  
--2.4.2 TODO
