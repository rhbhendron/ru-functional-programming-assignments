module Dice where

import System.Random
import Control.Monad
import Control.Applicative
import Data.List
import RandomState
import RandomGen
import LCG
import Data.Functor ((<&>))

data Expr = Lit Int | Dice Int 
          | Expr :+: Expr | Expr :-: Expr | Expr :/: Expr
          | Min Expr Expr | Max Expr Expr
  deriving (Show)

infixl 6 :+: 

type DiceAction m = Int -> m Int

evalDoBlock :: (Monad m) => Expr -> Expr -> DiceAction m -> (Int -> Int -> Int) -> m Int
evalDoBlock mx my act f = do
  x <- evalM mx act
  y <- evalM my act
  pure (f x y)

-- evalM :: Expr -> DiceAction IO -> IO Int             -- prototype
evalM :: (Monad m) => Expr -> DiceAction m -> m Int  -- final version
evalM (Lit x) _ = pure x
evalM (Dice mx) act = act mx >>= pure
evalM (mx :+: my) act = evalDoBlock mx my act (+)
evalM (mx :-: my) act = evalDoBlock mx my act (-)
evalM (mx :/: my) act = evalDoBlock mx my act div
evalM (Min mx my) act = evalDoBlock mx my act min
evalM (Max mx my) act = evalDoBlock mx my act max


evalRIO :: Expr -> IO Int
evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
 where report x = do { putStr "rolled a "; print x; pure x }

askInput :: DiceAction IO
askInput die = do
  putStr ("Please input a number between 0 and " ++ show die ++": ")
  line <- getLine
  return (min (read line :: Int) die)

evalIO :: Expr -> IO Int
evalIO expr = evalM expr askInput

listDiceAction :: DiceAction []
listDiceAction die = [1..die]


evalND :: Expr -> [Int]
evalND expr = evalM expr listDiceAction

avg :: (Fractional a) => [Int] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)


evalR :: Expr -> RandomState Int
evalR expr = evalM expr (\x -> getRandomRange (1,x))

observed :: (Fractional a) => Int -> Expr -> IO a
observed 1 expr = evalRIO expr <&> fromIntegral
observed n expr = let
    nf = fromIntegral n
    averageFactor = (nf-1)/nf
  in
    evalRIO expr >>= (\x -> observed (n-1) expr <&> (*averageFactor) <&> (+x)) . (/nf) . fromIntegral
