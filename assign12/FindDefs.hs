module FindDefs where
import Data.Functor
import Control.Monad

sayIO     :: IO Int -> IO String
sayIO m = m <&> show

sayMaybe  :: Maybe Int -> Maybe String
sayMaybe m = m <&> show

mpair     :: (Monad m) => m a -> m b -> m (a,b)
mpair x y = x >>= (\x -> y >>= (\z -> return (x,z)))

weirdBind :: (Monad m) => Maybe (m a) -> (a -> m b) -> m (Maybe b)
weirdBind mayB f = mayB >>= (f >=> (pure . Just))
