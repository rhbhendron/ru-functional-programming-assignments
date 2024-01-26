module Result where

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
  fmap f (Okay x)  = Okay (f x)
  fmap _ (Error m) = Error m

instance Applicative Result where
  pure = Okay
  Okay f   <*> Okay x   = Okay (f x)
  Error e1 <*> Error e2 = Error (e1 `union` e2)
  Error e1 <*> _        = Error e1
  _        <*> Error e2 = Error e2
