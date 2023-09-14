module Char where

import Data.Char

--2.5.1
(~~) :: String -> String -> Bool
(~~) x y = map toLower x == map toLower y

--2.5.2
reverseCase :: String -> String
reverseCase x = map reverseCase' x

reverseCase' :: Char -> Char
reverseCase' x = if isLower x then toUpper x else toLower x

--2.5.3
shift :: Int -> Char -> Char
shift x y
  | isUpper y = chr ((ord y - ord 'A' + x) `mod` 26 + ord 'A')
  | otherwise = y
  
--2.5.4
caesar :: Int -> String -> String
caesar x y = map (shift x) $ map toUpper y

--2.5.5
msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
--"FIRST I MUST SPRINKLE YOU WITH FAIRY DUST"
