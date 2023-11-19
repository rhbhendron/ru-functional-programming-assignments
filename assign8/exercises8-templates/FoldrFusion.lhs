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
  f ==> foldr p e
  g ==> step q
  h ==> p . q
  z ==> []

As follows:

  foldr p e . map q
              -----             rewrite map as foldr
= foldr p e . foldr (step q) []
  ----------------------------- foldr fusion
= foldr (p . q) (foldr p e [])
                --------------- definition of foldr
= foldr (p . q) e


Since the "THEN" part of the fusion law can only be applied if the "IF" part is true,
we need to show that for all x, y:

   foldr p e ((step q) x y) = (p . q) x (foldr p e y)

Which is the case since:

   foldr p e ((step q) x y)
             --------------  definition of step
 = foldr p e (q x : y)
   -------------------       definition of foldr
 = p (q x) (foldr p e y)
   -------                   definition of (.)
 = (p . q) x (foldr p e y)


----------------------------------------------
To prove:  map (f . g) = map f . map g

   map (f . g)
   ----------- definition of map
 = foldr (step (f . g)) []
   -------------------- by below lemma
 = foldr (step f . g) []
   --------------------- by foldr-map fusion
 = foldr (step f) [] . map g
   ----------------- definition of map
 = map f . map g



It can be helpful to also prove that:  step (f . g) = step f . g:

To prove: step (f . g) x xs = (step f . g) x xs

   step (f . g) x xs
   -----------------  definition of step
 = (f . g) x : xs
   ----------         definition of (.)
 = f (g x) : xs
   ------------       definition of step
 = step f (g x) xs
   ---------------    definition of (.)
 = (step f . g) x xs

----------------------------------------------
To prove:  mconcat . concat = mconcat . map mconcat

