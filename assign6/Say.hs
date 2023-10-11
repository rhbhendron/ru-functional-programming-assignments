module Say where

say :: Integer -> String
say  0 = "zero"
say  1 = "one"
say  2 = "two"
say  3 = "three"
say  4 = "four"
say  5 = "five"
say  6 = "six"
say  7 = "seven"
say  8 = "eight"
say  9 = "nine"
say 10 = "ten"
say 11 = "eleven"
say 12 = "twelve"

say 13 = "thirteen"
say 15 = "fifteen"

say 18 = "eighteen"

say 20 = "twenty"
say 30 = "thirty"
say 40 = "forty"
say 50 = "fifty"
say 80 = "eighty"
say 100 = "hundred"
say 1000 = "thousand"

say x
  |  x >= 13 && x /= 15 && x /= 18 && x <= 19 = say (x - 10) ++ "teen"
  |  x == 60 || x == 70 || x == 90            = say d10 ++ "ty"
  |  x >= 101  && x <= 999                    = say d100 ++ " " ++ say 100 ++ if m100 /= 0 then " " ++ say m100 else ""
  |  x >= 1010 && x <= 999999                 = say d1000 ++ " " ++ say 1000 ++ if m1000 /= 0 then " " ++ say m1000 else ""
  |  otherwise                                = (say $ floor $ realToFrac $ d10 * 10) ++ " " ++ say m10
  where 
    d10   = div x 10
    d100  = div x 100
    d1000 = div x 1000
    m10   = mod x 10
    m100  = mod x 100
    m1000 = mod x 1000
  
  
