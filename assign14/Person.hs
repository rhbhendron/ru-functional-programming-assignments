{-# LANGUAGE RankNTypes #-}
module Person where

import Lenses
import Data.Monoid

data Person = Person { name::String, age::Int, favouriteCourse::String }
  deriving (Eq,Show)

students :: [Person]
students = [elena,peter,pol,sjaak,frits,twan,marc]

elena, peter, pol, sjaak, frits, twan, marc :: Person
elena = Person "Elena" 33 "Functional Programming"
peter = Person "Peter" 57 "Imperative Programming"
pol   = Person "Pol"   36 "Object Oriented Programming"
sjaak = Person "Sjaak" 26 "Software Verification"
frits = Person "Frits" 61 "Functional Programming"
twan  = Person "Twan"  21 "Category Theory"
marc  = Person "Marc"  41 "Mathematics"

pretty :: Person -> String
pretty p = name p ++ " (" ++ show (age p) ++ "), likes " ++ favouriteCourse p

name' :: Lens Person String
name' f p = (\x->p{ name=x }) <$> f (name p)

--age' :: Lens Person Int

--favCourse' :: Lens Person String

sumOf :: (Num a) => Traversal s a -> s -> a
sumOf trav = error "TODO: implement me"

meanOf :: (Fractional n, Integral a) => Traversal s a -> s -> n
meanOf trav x = fromIntegral (sumOf trav x) / fromIntegral (lengthOf trav x)
