module FindDefs where

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = f x <$> (: (mapFilter f xs))

--lift      :: (a -> b -> Maybe c) -> (Maybe a -> Maybe b -> Maybe c)

--compute   :: (Num n) => (a -> n) -> [a] -> n

--fuse      :: (a -> b -> c) -> (a -> b) -> a -> c
