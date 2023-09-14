module PolyType where

--2.4.1 f8 can be used on arguments of type String
f8 x y  = if x <= y then x else y

f9 x y  = not x || y

f10 x y
  | x == 0    = y
  | otherwise = x + y

--2.4.1 f11 can be used on arguments of type String
f11 x y = get 0
  where get n = if n == 0 then x else y
  
--2.4.2
{-
f8 is Ad-Hoc Polymorphic, as it only works on the typeclass Ord

f9 is not polymorphic, as it only works on Booleans

f10 is Ad-Hoc Polymorphic, as it works on only the typeclass Num

f11 is Parametic Polymorphic, as it can return x or y, and there is no operations performed on either of them

-}
