module PolyTypes where

mingle :: [a] -> [a] -> [a]
mingle xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

sumWith :: (Num a, Num b) => (a -> b) -> [a] -> b
sumWith g xs = foldr (+) 0 (map g xs)

transform :: ([a] -> [b]) -> [[a]] -> [b]
transform f  = concat . map f
