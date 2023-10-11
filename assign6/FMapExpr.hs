module FMapExpr where

import Data.Char

-- ++ mapped to each item of the list
expr1 = fmap (\x->x+1) [1,2,3]

-- dr. prefixed to each item after, which is just sjaak in this case
expr2 = fmap ("dr." ++) (Just "Sjaak")

-- makes all items in list or string lowercase
expr3 = fmap toLower "Marc Schoolderman"

-- dr. prefixed to each item in the list, if there is an item.
expr4 = fmap (fmap ("dr." ++)) [Nothing, Just "Marc", Just "Twan"]
