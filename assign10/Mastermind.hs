module Main where

import Data.List
import Data.Char
import Control.Monad
import Data.Maybe
import System.Random
import System.IO

{---------------------- functional parts -------------------------------}

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
  deriving (Enum,Eq,Ord,Show)

score :: (Ord a) => (a,a) -> Int -> Int
score (a,b) x
  | a == b = x+1
  | otherwise = x

isInCode :: (Ord a) => [a] -> [a] -> Int
isInCode [] _ = 0
isInCode _ [] = 0
isInCode (x:xs) ys = isInCode xs (delete x ys) + if x `elem` ys then 1 else 0

finalScore :: (Int,Int) -> (Int, Int)
finalScore (x,y) = (x,y-x)

scoreAttempt :: (Ord a) => [a] -> [a] -> (Int,Int)
scoreAttempt code guess = finalScore (foldr score 0 (zip code guess), isInCode code guess)

-- Some test cases from: https://www.boardgamecapital.com/game_rules/mastermind.pdf
test1, test2, test3, test4, test5 :: Bool
test1 = scoreAttempt [1,2,3,4,5 :: Int] [2,6,3,7,8 :: Int] == (1,1)
test2 = scoreAttempt [1,2,3,4,2 :: Int] [5,6,3,3,7 :: Int] == (1,0)
test3 = scoreAttempt [1,2,1,3,3 :: Int] [4,1,5,6,7 :: Int] == (0,1)
test4 = scoreAttempt [4,1,5,6,7 :: Int] [1,2,1,3,3 :: Int] == (0,1)
test5 = scoreAttempt [4,1,5,1,7 :: Int] [1,2,1,3,3 :: Int] == (0,2)


getColour :: String -> Colour
getColour (x:xs)
  | toUpper x == 'R' = Red
  | toUpper x == 'B' = Blue
  | toUpper x == 'S' = Silver
  | toUpper x == 'G' = Green
  | toUpper x == 'O' = Orange
  | toUpper x == 'P' = Pink
  | toUpper x == 'Y' = Yellow
  | toUpper x == 'W' = White

colString :: [Colour] -> String
colString = unwords . map (++ ","). map show

{---------------------- IO parts -------------------------------}

-- only here for example; you can remove these from your final file

getCode :: Int -> IO [Colour]
getCode 0 = pure []
getCode n = do
  r <- randomRIO (0,7)
  let col = toEnum r :: Colour
  ys <- getCode (n - 1)
  pure (col : ys)

newPlayGame :: Int -> [Colour] -> IO ()
newPlayGame tries code = do
  putStrLn ("Please make a guess, you have " ++ show tries ++ " tries remaining")
  strGuess <- getLine
  let guess = (map getColour . words) strGuess
  let (exScore, loScore) = scoreAttempt code guess
  if exScore == length code
    then putStr "Correct"
    else do
      putStrLn (show exScore ++ " colour(s) in the correct position")
      putStrLn (show loScore ++ " colour(s) in the incorrect position")
      if (tries - 1) == 0
        then do
          putStrLn "Ran out of tries"
          putStr ("The correct answer was " ++ colString code)
        else newPlayGame (tries - 1) code

main :: IO ()
main = do
  code <- getCode 4
  newPlayGame 12 code
