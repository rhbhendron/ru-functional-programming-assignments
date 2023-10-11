module FMapExpr where

import Data.Char

expr1 = fmap (\x->x+1) [1,2,3]

expr2 = fmap ("dr." ++) (Just "Sjaak")

expr3 = fmap toLower "Marc Schoolderman"

expr4 = fmap (fmap ("dr." ++)) [Nothing, Just "Marc", Just "Twan"]
