module FindDefs where

(?$) :: Maybe (a -> b) -> Maybe a -> Maybe b
(Just f) ?$ (Just x) = Just (f x)
Nothing ?$ _ = Nothing
_ ?$ Nothing = Nothing

pair :: (Applicative f) => f a -> f b -> f (a,b)
pair x y = (,) <$> x <*> y

apply :: [a -> b] -> a -> [b]
apply xs x= (\f -> f x) <$> xs

apply2nd :: [a -> b -> c] -> b -> [a -> c]
apply2nd xs x = (\f a -> f a x) <$> xs
