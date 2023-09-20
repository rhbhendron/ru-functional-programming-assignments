module Mingle where

(++/) :: [a] -> [a] -> [a]
[] ++/ ys     = ys
(x:xs) ++/ ys = x:(ys ++/ xs)

infixr 5 ++/
