{-# LANGUAGE InstanceSigs #-}
import Control.Applicative
import Control.Monad


data ListMaybe a = LM [Maybe a]
  deriving (Eq, Show)

instance Functor ListMaybe where
  fmap :: (a -> b) -> ListMaybe a -> ListMaybe b
  fmap f (LM xs) = LM (map (fmap f) xs)
-- instance Applicative ListMaybe
-- instance Monad ListMaybe

test1, test2, test3, test4 :: ListMaybe Int
test1 = fmap succ (LM [Nothing])
      -- expected: LM [Nothing]
test2 = fmap succ (LM [Just 1, Just 2, Nothing])
      -- expected: LM [Just 2,Just 3,Nothing]
test3 = (+) <$> LM [Nothing, Just 2, Just 3] <*> LM [Just 1, Nothing]
      -- expected: LM [Nothing,Nothing,Just 3,Nothing,Just 4,Nothing]
test4 = liftM2 (+) (LM [Nothing, Just 2, Just 3]) (LM [Just 1, Nothing])
      -- expected: LM [Nothing,Just 3,Nothing,Just 4,Nothing]


data Compose f g a = C (f (g a))
  deriving (Eq, Show)

-- instance .. => Functor (Compose f g)
-- instance .. => Applicative (Compose f g)
-- instance .. => Monad (Compose f g)

