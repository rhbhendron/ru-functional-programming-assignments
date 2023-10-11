module Pronounceable where

import Data.List
import Say (say)

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'"++[c]++"'"]

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)
