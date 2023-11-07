> module Btree where
>
> data Btree a = Tip a | Bin (Btree a) (Btree a)
>
> mapBtree :: (a -> b) -> Btree a -> Btree b
> mapBtree f (Tip a)     = Tip (f a)
> mapBtree f (Bin t1 t2) = Bin (mapBtree f t1) (mapBtree f t2)

> tips :: (Btree a) -> [a]
> tips (Tip x) = [x]
> tips (Bin as bs) = tips as ++ tips bs

To prove: map f (tips t) = tips (mapBtree f t) for all f,t

In order to prove this, I must prove a helper lemma, which is

map f (a ++ b) = (map f a) ++ (map f b) for all f,a,b

Case: a = []

        map f ([] ++ b) = (map f []) ++ (map f b)
                                                {applying map}
        map f ([] ++ b) = [] ++ (map f b)
                                                {applying ++}
        map f b = map f b

Case: a = (x:xs)

IH:  map f (xs ++ b) = map f xs ++ map f b

        map f ((x:xs) ++ b) = (map f (x:xs)) ++ (map f b)
                                                        {applying map}
        map f ((x:xs) ++ b) = (f x : (map f xs)) ++ (map f b)
                                                        {applying ++}
        map f (x:(xs ++ b)) = f x : (map f xs ++ map f b)
                                                        {applying map}
        f x : (map f (xs ++ b)) = f x : (map f xs ++ map f b)
                                                        {applying IH}
        f x : (map f (xs ++ b)) = f x : (map f (xs ++ b))


Now that we have proved that, we can prove map f (tips t) = tips (mapBtree f t) for all f,t

Case: t = Tip a

        map f (tips (Tip a)) = tips (mapBtree f (Tip a))
                                                {applying mapBtree}
        map f (tips (Tip a)) = tips (Tip (f a))
                                                {applying tips}
        map f [a] = [f a]
                                                {applying map}
        [f a] = [f a]

Case: t = Bin t1 t2
using IH:
   map f (tips t1) = tips (mapBtree f t1)  and
   map f (tips t2) = tips (mapBtree f t2)

        map f (tips (Bin t1 t2)) = tips (mapBtree f (Bin t1 t2))
                                                                {applying mapBtree}
        map f (tips (Bin t1 t2)) = tips (Bin (mapBtree f t1) (mapBtree f t2))
                                                                {applying tips}
        map f (tips t1 ++ tips t2) = tips (mapBtree f t1) ++ tips (mapBtree f t2)
                                                                {applying lemma}
        map f (tips t1) ++ map f (tips t2) = tips (mapBtree f t1) ++ tips (mapBtree f t2)
                                                                {applying IH}
        map f (tips t1) ++ map f (tips t2) = map f (tips t1) ++ map f (tips t2)
