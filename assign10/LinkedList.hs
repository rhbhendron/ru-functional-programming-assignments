module LinkedList where

import Data.IORef
import Control.Monad

type ListRef elem = IORef (List elem)

data List elem = Nil | Cons elem (ListRef elem)

nil :: IO (ListRef elem)
nil = newIORef (Nil)

cons :: elem -> ListRef elem -> IO (ListRef elem)
cons x xs = do
  list <- xs
  pure (Cons x xs)

-- fromList :: [elem] -> IO (ListRef elem)
-- fromList xs = foldM cons nil xs

toList :: ListRef elem -> IO [elem]
toList = error "toList: not yet implemented"

foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b)
foreach = error "foreach: not yet implemented"

--inplace_foreach :: ???
