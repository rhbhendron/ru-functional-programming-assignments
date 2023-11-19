module Minimax where

type Position = (Integer, Integer)

-- moves :: Position -> [Position]

-- Multiway trees.

data Tree elem  =  Node elem [Tree elem]

-- gametree :: (position -> [position]) -> (position -> Tree position)

-- size :: Tree elem -> Integer

-- winning  :: Tree position -> Bool
-- losing   :: Tree position -> Bool

-- evaluate :: Integer -> Position -> Value
-- evaluate depth  =  maximize static . prune depth . gametree moves

-- prune :: Integer -> Tree elem -> Tree elem

type Value = Int  -- |[-100 .. 100]|

-- static :: Position -> Value

-- maximize  :: (position -> Value) -> (Tree position -> Value)
-- minimize  :: (position -> Value) -> (Tree position -> Value)
