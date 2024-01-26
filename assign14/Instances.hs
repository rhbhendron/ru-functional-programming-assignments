{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Instances where

import Result

-- you should only implement *either* foldr or foldMap; doing both is unnecessary
instance Foldable Result where
  --foldr :: (a -> b -> b) -> b -> Result a -> b
  --foldMap  :: (Monoid m) => (a -> m) -> Result a -> m

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error msg) = pure (Error msg)
  traverse f (Okay x)    = Okay <$> f x

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq,Show)

--instance Foldable Tree where
  --foldMap :: ...

--instance Functor Tree where
  --fmap :: ...

--instance Traversable Tree where
--  traverse :: ...

assistants :: Tree String
assistants = Node "Patrick" 
               (Node "Jen" 
                  (Node "Cassian" (Node "Bram" Leaf Leaf) Leaf) 
                  (Node "Mario" Leaf Leaf))
               (Node "Sander" 
                  (Node "Rico" (Node "Quinten" Leaf Leaf) Leaf)
                  (Node "Willem" Leaf Leaf))

flatten :: (Foldable t) => t String  -> String
flatten = foldr1 (\x y->x++", "++y)
