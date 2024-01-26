module MaybeMonad where

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap = (<$>)

stripMaybe :: Maybe (Maybe a) -> Maybe a
stripMaybe x = x >>= id

applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f x= x >>= f
