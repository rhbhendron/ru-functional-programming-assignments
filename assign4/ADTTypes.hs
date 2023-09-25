module ADTTypes where

import ADTs

--isWeekend :: ???
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

--bval :: ???
bval prop = case prop of
              T         -> True
              F         -> False
              (p :-> q) -> not (bval p) || bval q


--incr :: ???
incr (Bar k) = Bar (unlines k)

--wrap :: ??? Integer -> a -> Wrapped a
wrap 0 x = Bare x
wrap n x = wrap (n-1) (Wrapped x)
