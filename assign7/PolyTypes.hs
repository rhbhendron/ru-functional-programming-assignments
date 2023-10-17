module PolyTypes where

justs xs           = [ x | Just x <- xs, x /= ' ' ]

orderPairs xs      = map (\(x,y)->(min x y, max x y)) xs

unmaybe (Just x)   = x
unmaybe Nothing    = Nothing

accumulate f st    = let (x,st') = f st in x : accumulate f st'
