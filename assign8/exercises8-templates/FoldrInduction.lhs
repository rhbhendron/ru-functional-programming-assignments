Local definitions:

> import Prelude hiding (foldr)

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr f b [] = b
> foldr f b (x:xs) = f x (foldr f b xs)
>
> compose :: [a -> a] -> a -> a
> compose [] = id
> compose (f:fs) = f . compose fs

-----------------------------------------------------
To prove: foldr f b xs = compose (map f xs) b
By induction on xs.

Case 1: ...

Case 2: ...
IH: ...
