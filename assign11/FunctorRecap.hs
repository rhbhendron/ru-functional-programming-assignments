{-# LANGUAGE InstanceSigs #-}

data TwoOrThree a = Two a a | Three a a a
  deriving (Eq, Show)

instance Functor TwoOrThree where
  fmap f (Two x y) = Two (f x) (f y)
  fmap f (Three x y z) = Three (f x) (f y) (f z)

-- fmap succ (Two 1 2)} = Two 2 3

doubleAll :: [TwoOrThree Int] -> [TwoOrThree Int]
doubleAll = map (fmap (* 2))


data TwoOrThree' a = Two' Int a | Three' Int a a
  deriving (Eq, Show)

instance Functor TwoOrThree' where
  fmap f (Two x y) = Two x (f y)
  fmap f (Three x y z) = Three x (f y) (f z)

-- fmap succ (Two' 1 2) =

-- instance Functor Int is not possible, as Int is a type of kind *, not * -> *
