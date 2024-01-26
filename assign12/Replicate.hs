module Replicate where

-- this is the definition in the slides:
replicateM' :: (Monad m) => Int -> m a -> m [a]
replicateM' 0 _  = return []
replicateM' n mx = (:) <$> mx <*> replicateM' (n-1) mx

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM 0 _  = return []
--replicateM n mx = do
--  ...
