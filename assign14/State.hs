module State where

import Control.Monad

{- Note: the definitions below are more "easy" (those are scare quotes) to understand versions of the
 - official MonadState monad, which is in Control.Monad.State.
 - (The official MonadState is harder as it uses an extra monad to wrap the state in)
 -}

newtype State s a = St { runState :: s -> (a, s) }

--evaluate inside the state monad, but throw away the state afterwards
evalState :: State s a -> s -> a
evalState st g = fst (runState st g)

--evaluate inside the state monad purely for the side effect on the state
execState :: State s a -> s -> s
execState st g = snd (runState st g)

--place a "state transformation" function directly
state :: (s -> (a, s)) -> State s a
state f = St f

--get and put the state
get :: State s s
get = St $ \s -> (s,s)

put :: s -> State s ()
put s = St $ \_ -> ((), s)

instance Functor (State s) where
  fmap = liftM

-- boilerplate instance of Applicative
instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return x = St $ \s -> (x, s)
  sx >>= k = St $ \s1 -> let (x, s2) = runState sx s1
                             (y, s3) = runState (k x) s2
                         in  (y, s3)
