module ListHOF where

import Data.List
import Data.Char
import Data.Function

sortLength :: [String] -> [String]
sortLength = sortOn length

letterClump :: String -> [String]
letterClump = groupBy (\x y -> isAlphaNum x && isAlphaNum y)

-- note: test this with 'take 20 fibs'
--fibs :: [Integer]

--zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
