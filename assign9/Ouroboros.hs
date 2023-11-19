module Ouroboros where

import Data.Maybe

type AssocList k a = [(k,a)]

insertLookup :: (Eq k) => k -> a -> AssocList k a -> (Maybe a, AssocList k a)
insertLookup k v [] = (Nothing, [(k,v)])
insertLookup k v ((k1,v1):xs) = (first, second) where
  first = if k == k1 then Just v1 else Nothing
  second = if k == k1 then

--insert :: (Eq k) => k -> a -> AssocList k a -> AssocList k a
--insert k v kvs = kvs'
--  where (_, kvs') = insertLookup k v kvs

--lookup :: (Eq k) => k -> AssocList k a -> Maybe a
--lookup k kvs   = x
--  where (x, _) = insertLookup k undefined kvs

--adjust :: (Eq k) => (a -> a) -> k -> AssocList k a -> AssocList k a
