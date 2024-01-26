module AskNames where

generateNames :: [String] -> [String] -> [String]
generateNames firstnames surnames = [ f ++ " " ++ l | f <- firstnames, l <- surnames ]

getFullName :: IO String
getFullName = do
  first   <- putStrLn "First name?" >> getLine
  surname <- putStrLn "Last name?"  >> getLine
  return (first ++ " " ++ surname)

makeName :: (Applicative t) => t String -> t String -> t String
makeName first surname = pure (\f l->f ++ " " ++ l) <*> first <*> surname
