module RandomGen where

import RandomState
import LCG

import System.Random
import Control.Monad

getRandomRange :: (Int,Int) -> RandomState Int
getRandomRange (a,b) = (state . randomRange) (a,b)


--multiEval :: [RandomState a] -> RandomState [a]
--multiEval xs = do ..

roll_2d6 :: RandomState Int
roll_2d6 = do
  a <- getRandomRange (1,6)
  b <- getRandomRange (1,6)
  pure (a+b)

runRandomStateIO :: RandomState a -> IO a
runRandomStateIO action = do
  seed <- randomIO
  let new_seed = mkSeed seed
  let (x,_) = runState action new_seed
  return x

--these definitions can be used to test your function a bit more thoroughly
runRandomNumbers :: (Int,Int) -> Int -> Seed -> [Int]
runRandomNumbers range n seed = result
  where (result, _) = runState (replicateM n (getRandomRange range)) seed

testme :: [Int]
testme = runRandomNumbers (0,999) 100 (mkSeed 42)

{-
  12.4.3
    sequence [roll_2d6,roll_2d6,roll_2d6] is type RandomState [Int].

    This will perform the roll_2d6 action 3 times, and stores the state of
    each in a seperate part of the list

-}
