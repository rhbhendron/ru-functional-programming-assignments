{-# LANGUAGE RankNTypes #-}
module Lenses where

import Control.Applicative
import Data.Monoid
import Data.Functor.Identity

{- generalizing the idea of traversable to traversals -}

type Traversal s a = forall f. (Applicative f) => (a -> f a) -> (s -> f s)

foldMapOf :: (Monoid m) => Traversal s a -> (a -> m) -> (s -> m)
foldMapOf trav f = getConst . trav (Const . f)

lengthOf :: Traversal s a -> s -> Int
lengthOf trav = getSum . foldMapOf trav (\_->Sum 1)

mapOf :: Traversal s a -> (a -> a) -> (s -> s)
mapOf trav f = runIdentity . trav (Identity . f)

foldOf :: (Monoid m) => Traversal s m -> s -> m
foldOf trav = foldMapOf trav id

listOf :: Traversal s a -> s -> [a]
listOf trav = foldMapOf trav (\x->[x])

{- some predefined traversals -}

each :: (Traversable t) => Traversal (t a) a
each = traverse

at :: (Applicative f) => Int -> (a -> f a) -> [a] -> f [a]
at 0 f (x:xs) = (:xs) <$> f x
at n f (x:xs) = (x:)  <$> at (n-1) f xs
at _ _ _ = error "index out of range"

evens :: (Applicative f) => (a -> f a) -> [a] -> f [a]
evens _ [] = pure []
evens f (x:xs) = (:) <$> f x <*> odds f xs

odds :: (Applicative f) => (a -> f a) -> [a] -> f [a]
odds _ [] = pure []
odds f (x:xs) = (x:) <$> evens f xs

when :: (a -> Bool) -> Traversal a a
when predicate f x
  | predicate x = f x
  | otherwise   = pure x

{- lenses -}

type Lens s a = forall f. (Functor f) => (a -> f a) -> (s -> f s)

view :: Lens s a -> s -> a
view lens = getConst . lens Const

modify :: Lens s a -> (a -> a) -> s -> s
modify = mapOf

set :: Lens s a -> a -> s -> s
set lens x = modify lens (const x)

{- some sample lenses -}
fst' :: (Functor f) => (a -> f a) -> (a,b) -> f (a, b) -- Lens (a,b) a
fst' f (x,y) = (\x' -> (x',y)) <$> f x
snd' :: (Functor f) => (b -> f b) -> (a,b) -> f (a, b) -- Lens (a,b) b
snd' f (x,y) = (\y' -> (x,y')) <$> f y
