{-# LANGUAGE DeriveFunctor, InstanceSigs, FlexibleInstances #-}
module Parser where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Char

type ParseResult t = Either String t
newtype Parser a = P { parse :: String -> ParseResult (a, String) }
  deriving (Functor)

--note: "parse" is the accessor function with type:
--parse :: Parser a -> (String -> Maybe (a, String))
--
--i use it as an infix operator in the code below to emphasise what is
--doing the perasing (left argument) and what is being parsed (right-hand side)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x  = P (\inp -> Just (x,inp))

--(<*>) :: Parser (a -> b) -> Parser a -> Parser b
--fp <*> xp = P (\inp1 -> case parse fp inp1 of
--                          Nothing        -> Nothing
--                          Just (g, inp2) -> parse (fmap g xp) inp2)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fp <*> xp = P (\inp1 -> do -- equivalent to the above, but using the Maybe monad and do-notation
                   (g, inp2) <- parse fp inp1
                   (y, inp3) <- parse xp inp2
                   pure (g y, inp3))

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp1 -> do
                 (x, inp2) <- parse p     inp1
                 (y, rest) <- parse (f x) inp2
                 pure (y, rest))

instance Alternative Parser where
  empty :: Parser a
  empty = P (\_ -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2  = P (\inp-> p1 `parse` inp <|> p2 `parse` inp)

parseAll :: Parser a -> String -> Maybe a
parseAll p inp = case parse p inp of
                   Just (x,[]) -> Just x
                   _          -> Nothing

-- this is a parser which only accepts the "empty language"; i.e. it never matches
failure :: Parser a
failure = empty

{- low level primitives (usually handled by "lexers" in more complex parsing jobs) -}

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- getchar
           if p c then pure c
           else failure
  where 
  getchar :: Parser Char -- a "parser" that always succeeds if there is a character of input
  getchar = P uncons
  
digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isAlpha

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = pure []
string (x:xs) = pure (:) <*> char x <*> string xs

ident :: Parser String
ident = (:) <$> lower <*> many alphanum

nat :: Parser Int
nat = read <$> some digit

int :: Parser Int
int = negate <$ char '-' <*> nat 
  <|> nat

space :: Parser ()
space = many (sat isSpace) *> pure ()

token :: Parser a -> Parser a
token p = space *> p <* space

{- grammar level primitives (which eat whitespace) -}

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

identifier :: Parser String
identifier = token ident

symbol :: String -> Parser String
symbol xs = token (string xs)

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = p `sepBy1` sep <|> pure []

times :: Int -> Parser a -> Parser [a]
times = replicateM
