module FunList where

--define using the _list design pattern_
compose :: [a -> a] -> (a -> a)
compose [x] = x
compose (x:xs) = x . (compose xs)

--define using `foldr`
compose' :: [a -> a] -> (a -> a)
compose' = foldr (.) id

--Explain _what_ the following function computes, and _how_ it computes it
{-

In this function finds the factorial of the number n. It does this by composing
1 * 2 * 3 * .... * n, and then applying this number to 1

-}
foo :: (Integral n) => n -> n
foo n = compose (map (*) [1..n]) 1

--define in terms of *only* `map` and `compose`
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f i xs = compose (map f xs) i
