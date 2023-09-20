module ListComprehensions where

--3.3.6.1 & 3.3.6.2

--computes a list of pairs by taking two input lists as and bs and forming pairs of elements from each list, where each element from as is paired with each element from bs.
concatPairs :: [a] -> [b] -> [(a, b)]
concatPairs as bs = [ (a,b) | a <- as, b <- bs ]

--generates a list of size n containing the value y repeated n times.
repeatValue :: (Num t, Enum t) => t -> a -> [a]
repeatValue n y   = [ y | i <- [1..n] ]

--takes an integer n and a list xs and returns the first n elements from the list xs.
takeFirstN :: (Num a1, Enum a1, Ord a1) => a1 -> [a2] -> [a2]
takeFirstN n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]

--takes an element a and a list xs, and returns the indices of all occurrences of a in the list xs.
findIndices' :: (Num a1, Enum a1, Eq a2) => a2 -> [a2] -> [a1]
findIndices' a xs  = [ i | (i,x) <- zip [0..] xs, x == a]

--takes two lists xs and ys and interleaves their elements, resulting in a single list containing elements from both input lists in an alternating fashion.
interleaveLists :: [a] -> [a] -> [a]
interleaveLists xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

--flattens a list of lists xss into a single list by concatenating all the inner lists.
flattenList :: [[a]] -> [a]
flattenList xss   = [ x | xs <- xss, x <- xs ]

{-
repeatValue, takeFirstN, findIndices, and interleaveLists are overloaded using type classes. For example, findIndices uses the Eq type class to compare elements for equality.
concatPairs and flattenList are fully polymorphic and do not rely on type classes. They can work with elements of any type.
-}