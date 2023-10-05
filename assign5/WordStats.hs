import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M
--  ^^comment out this line if it causes compile errors (we need the `containers` library)

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x->(head x,length x)) . group . sort . words

mostFrequentOfLength :: Int -> String -> [String]
mostFrequentOfLength n = map fst . sortBy (flip compare) . filter (\x -> snd x >= n) . wordFrequency

sumUpTuples :: [(Int,Int)] -> (Int,Int)
sumUpTuples xs = ((fst . head) xs, (sum . map snd) xs)

wordLengthFrequency :: String -> [(Int,Int)]
wordLengthFrequency =  map sumUpTuples . groupBy (\x y -> fst x == fst y) .
  sortOn fst . map (\(x,y) -> ( length x, y)) . wordFrequency


anagrams :: String -> [[String]]
anagrams = foldr (\x y -> if length x >= 2 then x:y else y) [] . map nub .
  groupBy (\x y -> sort x == sort y) . sortOn sort . words

{- this 'main' function is only here for the compiled, stand-alone version
 - calling it from within GHCi is problematic since GHCi itself needs stdin!
 - to compile, run:
 -
 -     ghc -O WordStats
 -
 - (The -O flag turns on optimizations)
 -}
main :: IO ()
main = onStdin $ anagrams  -- change this to run a different function from the commandline
  where onStdin f = getContents >>= (mapM_ print . f . filter (\x->isAlphaNum x || isSpace x))
