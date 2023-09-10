module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
--1.5.1
person   =  ("Frits",   20,  "Course")

students :: [Person]
students = [elena, peter, pol, person]

age :: Person -> Age
age (_, n, _)  =  n

--1.5.2
name :: Person -> Name
name (n, _, _)  =  n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, n)  =  n

--1.5.3
showPerson :: Person -> String
showPerson p = "Age: " ++ show (age p) ++ ", name: " ++ name p ++ ", favourite course: " ++ favouriteCourse p

--1.5.4
twins :: Person -> Person -> Bool
twins x y = age x == age y

--1.5.5
increaseAge :: Person -> Person
increaseAge (n, a, c) = (n, a+1, c)

--1.5.6
query1 :: [Person]
--"increment the age of all students by two"
query1 = map increaseAge (map increaseAge students)

query2 :: [Person]
--"promote all of the students"
query2 = map promote students 

promote :: Person -> Person
promote (n, a, c) = ("dr. " ++ n, a, c)

query3 :: [Person]
--"all students named Frits"
query3 = let fritsFilter p = name p == "Frits" in filter fritsFilter students

query4 :: [Person]
--"all students who are in their twenties"
query4 = let ageFilter p = age p >= 20 && age p < 30 in filter ageFilter students

query5 :: Age
-- "the average age of all students"
query5 = sum (map age students) `div` (fromIntegral (length students))

query6 :: [Person]
-- "promote the students whose favourite course is Functional Programming"
query6 = map promoteFp students where
  promoteFp s = if favouriteCourse s == "Functional Programming" then promote s else s
