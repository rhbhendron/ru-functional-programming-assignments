module ListComprehensions where

g0 as bs = [ (a,b) | a <- as, b <- bs ]

g1 n y   = [ y | i <- [1..n] ]

g2 n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]

g3 a xs  = [ i | (i,x) <- zip [0..] xs, x == a]

g4 xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

g5 xss   = [ x | xs <- xss, x <- xs ]
