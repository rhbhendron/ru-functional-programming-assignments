module Empty where

{- which ones of these functions are the improper way to test for [] ? -}

isEmpty0 list = length list == 0

isEmpty1 list = list == []

isEmpty2 list = null list

isEmpty3 []   = True
isEmpty3 _    = False

someInts :: [Int]
someInts = [1..32]

manyInts :: [Int]
manyInts = [1..2^(27::Int)]

infiniteInts :: [Int]
infiniteInts = [1..]

nothingAtAll :: [a]
nothingAtAll = []

someFunctions :: [Int->Int->Int]
someFunctions = [(+), (*), mod, div]
