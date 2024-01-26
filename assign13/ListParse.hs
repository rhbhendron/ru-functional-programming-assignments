{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
 -}

intList :: Parser [Int]
intList = char '{' *> many (token int) <* char '}'


{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

someN :: Int -> Parser [Int]
someN 0 = pure []
someN n = do
  x <- token int
  xs <- someN (n - 1)
  pure (x:xs)


intRecord :: Parser [Int]
intRecord = do
  char '{'
  n <- nat <* char '#'
  list <- someN n
  char '}'
  pure list
