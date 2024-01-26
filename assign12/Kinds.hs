-- Pair :: ...
data Pair a = Pair a a

-- Tuple :: ...
data Tuple a b c = Tuple a b c

-- MList :: ...
data MList m a = Nil | Cons (m (a, MList m a))

-- Compose :: ...
newtype Compose f g a = Compose { runCompose :: f (g a) }

-- Funkytor :: ...
class Funkytor f where
  funkymap :: (a -> b) -> (b -> a) -> f a b -> f b a

-- ToList :: ...
class ToList a where
  toList :: a -> [a]

