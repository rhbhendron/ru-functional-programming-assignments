module LCG(Seed, mkSeed, randomInt, randomRange, randomList) where

import Data.List

newtype Seed = Seed Int

instance Show Seed where
  show (Seed s) = "mkSeed " ++ show s

-- we forbid direct access to the Seed constructor,
-- so this is the only function that allows the user to provide an initial value
mkSeed :: Int -> Seed
mkSeed = Seed

-- this function generators one random value (between 0 and 2^31-1) from a given seed, and gives a new seed
randomInt :: Seed -> (Int, Seed)
randomInt (Seed seed) = (seed' `div` 0x10000, Seed seed')
  where 
  seed' = (a * seed + c) `mod` m
  m = 2^31
  a = 1103515245
  c = 12345

-- convenience function: generate a random value within a certain range
randomRange :: (Integral n) => (Int,Int) -> Seed -> (n, Seed)
randomRange range seed = let (x, seed') = randomInt seed in (bounded range x, seed')

-- convenience function: just generate a lazy list of random numbers
randomList :: (Integral n) => (Int,Int) -> Seed -> [n]
randomList range = map (bounded range) . unfoldr (pure . randomInt)

-- convenience function: clip a number to certain range and convert it
bounded :: (Integral n) => (Int, Int) -> Int -> n
bounded (lo,hi)
  | lo > hi   = error "invalid range passed to 'bounded'"
  | otherwise = fromIntegral . \x -> (x `mod` (hi-lo+1)) + lo
