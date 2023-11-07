> module MonoidInduction where
> import Prelude hiding (foldl,foldr,mconcat)
>
> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr _ b [] = b
> foldr f b (x:xs) = f x (foldr f b xs)

> foldl :: (b -> a -> b) -> b -> [a] -> b
> foldl _ b [] = b
> foldl f b (x:xs) = foldl f (f b x) xs
>
> mconcat :: (Monoid a) => [a] -> a
> mconcat = foldr (<>) mempty

-------------------------------------------------------
To prove: mconcat (xs ++ ys) = mconcat xs <> mconcat ys


-------------------------------------------------------
To prove: foldl (<>) (x<>y) xs = x <> foldl (<>) y xs


-------------------------------------------------------
To prove: foldl (<>) mempty xs = foldr (<>) mempty xs

