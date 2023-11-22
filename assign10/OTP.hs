-- ghc --make OTP.hs
module Main where

import System.Random
import System.Environment

randInt :: IO Int
randInt = randomRIO (0,255)

main :: IO ()
main = do
  setStdGen (mkStdGen 1337)
  args <- getArgs
  let n = read (head args)
  randomNumbers <- sequence (replicate n randInt)
  print randomNumbers
  putStrLn ("average: " ++ show (sum randomNumbers `div` n))
