module Pronounceable where

import Data.List
import Say (say)

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'"++[c]++"'"]

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)

--4.7.1 and 4.7.2
instance Pronounceable Integer where
  pronounce :: Integer -> String
  pronounce = say

instance Pronounceable Int where
  pronounce n = say (toInteger n)

--4.7.3
instance Pronounceable Double where
  pronounce n =
    let integerPart = truncate n
        decimalPart = round $ 10 * (n - fromIntegral integerPart)
    in say integerPart ++ " point " ++ say decimalPart

--4.7.4
instance (Pronounceable a, Pronounceable b) => Pronounceable (a, b) where
  pronounce (a, b) = "a tuple containing " ++ pronounce a ++ " and " ++ pronounce b
