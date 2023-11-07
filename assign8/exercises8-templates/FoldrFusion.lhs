> module FoldrFusion where
>
> import Prelude hiding (map)

The foldr fusion law that

for all
  f :: b -> c
  g :: a -> b -> b
  h :: a -> c -> c
  z :: b
IF, for all x,y:
  f (g x y) = h x (f y)
THEN
  f . foldr g z = foldr h (f z)

----------------------------------------------

We can define map in terms of foldr:

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (step f) []
>   where step f x xs = f x : xs

----------------------------------------------

To prove:  foldr p e . map q = foldr (p . q) e

We can apply the fusion law using
  f ==> TODO
  g ==> TODO
  h ==> TODO
  z ==> TODO

As follows:

  foldr p e . map q
              -----             rewrite map as foldr
= foldr p e . foldr (step f) TODO
  ----------------------------- foldr fusion
= TODO
  ...
= foldr (p . q) e


Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that for all x, y:

   TODO (TODO x y) = TODO x (f y)

Which is the case since:

   ...


----------------------------------------------
To prove:  map (f . g) = map f . map g


It can be helpful to also prove that:  step (f . g) = step f . g:


----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

