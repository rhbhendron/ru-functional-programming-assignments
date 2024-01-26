module ApplicativeExpr where

expr1 = ("dr." ++) <$> Just "Sjaak"

expr2 = pure (filter (\x->x>1)) <*> Just [1,2,3]

expr3 = filter (>1) <$> Just [1,2,3]

expr4 = mod <$> Just 7 <*> Just 5

expr5 = replicate <$> [1,2,3] <*> ['a','b']



