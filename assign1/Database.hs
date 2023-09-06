module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")

students :: [Person]
students = [elena, peter, pol]

age :: Person -> Age
age (_, n, _)  =  n

-- name             :: Person -> Name
-- favouriteCourse  :: Person -> FavouriteCourse
-- showPerson       :: Person -> String
-- twins            :: Person -> Person -> Bool
-- increaseAge      :: Person -> Person

-- first develop the expressions in GHCi, then replace the TODO's below with them
query1 :: [Person]
query1 = _TODO "increment the age of all students by two"

query2 :: [Person]
query2 = _TODO "promote all of the students"

query3 :: [Person]
query3 = _TODO "all students named Frits"

query4 :: [Person]
query4 = _TODO "all students who are in their twenties"

query5 :: Age
query5 = _TODO "the average age of all students"

query6 :: [Person]
query6 = _TODO "promote the students whose favourite course is Functional Programming"

-- if you have removed all TODO's, remove these lines
_TODO :: String -> a
_TODO msg = error ("TODO: " ++ msg)
