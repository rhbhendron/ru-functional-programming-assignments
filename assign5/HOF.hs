module HOF where

import Prelude hiding (const)

{- exercise 5.1 -}

const x _y   = x

x $-> y      = y x

oper "mul" n = (*n)
oper "div" n = (n/)
oper _     _ = error "not implemented"

mapMap f xs  = map (map f) xs

without p    = filter (not . p)

on f g x y   = f (g x) (g y)

{- exercise 5.2 -}

f1 :: (Num a) => a -> a
f1 = (* 5) . (+ 1)

f2 :: (Num a) => a -> a
f2 = (+ 1) . (* 5)

f3 :: (Num a, Ord a) => a -> a
f3 = (min 100) . (max 0)

f4 :: [a] -> Bool
f4 = (<5) . length

