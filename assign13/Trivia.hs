module Trivia where

import Control.Applicative
import Parser

dot :: Parser Char
dot = char '.'

-- loop :: Parser Char
-- loop = many (many char)

dots :: Parser String
dots = (++) <$> many dot <*> many dot
