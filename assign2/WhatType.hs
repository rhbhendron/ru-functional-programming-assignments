module WhatType where

f0 (x,y)   = x == 'F' && y == 'P'

f1 s       = s ++ ", cruel world!"

f2 x (y,z) = (z,x,y)

f3 ' '     = '_'
f3 c       = c

f4 x y
  | x == ""   = y
  | otherwise = x

f5 b x y   = if b then (x,y) else (y,x)

f6 x       = \y -> x

f7         = ("Haskell" ++)
