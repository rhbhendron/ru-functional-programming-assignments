module ADTs where

data Day       = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

data Prop      = Prop :-> Prop | T | F
  deriving Show

data Unit      = Unit
  deriving Show

data Foo a     = Bar a
  deriving Show

data Tuple a b = Two a b | One a | None
  deriving Show

data Wrapped a = Wrapped (Wrapped a) | Bare a
  deriving Show
