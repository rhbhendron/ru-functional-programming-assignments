module Trees where

repeatSpaces :: Int -> String
repeatSpaces 0 = ""
repeatSpaces n = " " ++ repeatSpaces (n-1)

triangleLine :: Int -> String
triangleLine 1 = "*"
triangleLine n = "*" ++ triangleLine (n-1) ++ "*"

triangleSpaces :: Int -> Int -> String
triangleSpaces 0 j = ""
triangleSpaces n j = triangleSpaces (n-1) (j+1) ++ repeatSpaces j ++ triangleLine n ++ "\n"

triangle :: Int -> String
triangle n = triangleSpaces n 0

christmasTree :: Int -> String
christmasTree n = christmasTreeSpaces n 0 where
  christmasTreeSpaces 0 j = ""
  christmasTreeSpaces n j = christmasTreeSpaces (n-1) (j+1) ++ triangleSpaces n j
