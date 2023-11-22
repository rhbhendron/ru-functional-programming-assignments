module LinkedList where

import Data.IORef

type ListRef elem = IORef (List elem)

data List elem = Nil | Cons elem (ListRef elem)

nil :: IO (ListRef elem)
nil = return (Nil)

cons :: elem -> ListRef elem -> IO (ListRef elem)
cons x xs = error "not Implemented"

fromList :: [elem] -> IO (ListRef elem)
fromList = error "fromList: not yet implemented"

toList :: ListRef elem -> IO [elem]
toList = error "toList: not yet implemented"

foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b)
foreach = error "foreach: not yet implemented"

--inplace_foreach :: ???
