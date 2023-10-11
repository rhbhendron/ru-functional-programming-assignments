module Pronounceable where

import Data.List
import Say (say)

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'"++[c]++"'"]

instance Pronounceable Integer where
  pronounce c = unwords ["the","number", "'"++say c++"'"]

instance Pronounceable Int where
  pronounce c = unwords ["the","number", "'"++say (toInteger c)++"'"]

--instance Pronounceable Double where

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)
