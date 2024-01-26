module PolyTypes where

lift0 f x          = f x
lift1 f g1 x       = f (g1 x)
lift2 f g1 g2 x    = f (g1 x) (g2 x)

deMorgan quantor p   = not . quantor (not . p)
