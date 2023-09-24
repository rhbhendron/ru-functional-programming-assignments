module Obfuscate where

import Data.Char
import Data.List
import System.Random

cambridge :: String -> String
cambridge = unwords . shuffle . words


isSpaceOrPunc :: Char -> Bool
isSpaceOrPunc c = elem c ['.',',',' ']

myWords :: String -> [String]
myWords s = case dropWhile isSpaceOrPunc s of
              "" -> []
              s' -> w : myWords s''
                        where (w, s'') = break isSpaceOrPunc s'

-- scramble :: String -> String
-- scramble "" = ""
-- scramble s = [head s] ++ {- stuff-} ++ [last s]

permLen :: [a] -> Int
permLen x = (length $ permutations x) - 1

randIndex :: StdGen -> [a] -> (Int,StdGen)
randIndex g xs = randomR (0, permLen xs) g

scrambleInner :: StdGen -> String -> (String,StdGen)
scrambleInner g s = (permutations s !! index, gen) where
                        (index, gen) = randIndex g s

middleList :: [a] -> [a]
middleList = init . tail

-- scramble :: String -> String
scramble :: String -> StdGen -> (String, StdGen)
scramble [x] g = ([x], snd $ randIndex g [x])
scramble [x,y] g = ([x,y], snd $ randIndex g [x,y])
scramble [x,y,z] g = ([x,y,z], snd $ randIndex g [x,y,z])
scramble xs g
  | isSpaceOrPunc $ last xs = ([head xs] ++ string1 ++ [last $ init xs] ++ [last xs], gen1)
  | otherwise = ([head xs] ++ string2 ++ [last xs], gen2)
  where
    (string1, gen1) = scrambleInner g (middleList $ init xs)
    (string2, gen2) = scrambleInner g (middleList xs)

shuffleInner :: StdGen -> [String] -> [String]
shuffleInner _ [] = []
shuffleInner g (s:xs) = string : shuffleInner gen xs where
  (string, gen) = scramble s g

shuffle :: [String] -> [String]
shuffle (s:xs) = shuffleInner (mkStdGen $ permLen s) (s:xs)


meme :: String
meme = "According to research at Cambridge university, it doesn't matter\
       \ what order the letters in a word are, the only important thing is\
       \ that the first and last letters are at the right place. The rest can\
       \ be a total mess and you can still read it without a problem. This is\
       \ because we do not read every letter by it self but the word as a wohle."
