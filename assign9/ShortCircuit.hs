module ShortCircuit where

andl, andr, orl, orr :: [Bool] -> Bool

andl = foldl (&&) True
andr = foldr (&&) True
orl  = foldl (||) False
orr  = foldr (||) False

e1, e2, e3, e4 :: Bool

e1 = andl $ False : [True,  True  ..]
e2 = andr $ False : [True,  True  ..]
e3 = orl  $ True  : [False, False ..]
e4 = orr  $ True  : [False, False ..]
