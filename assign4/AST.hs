module AST where

--start with either:
--  data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | ...
--or:
--  data Expr = Lit Integer | Expr :+: Expr | Expr :*: Expr | ...
--  infixl 6 :+:
--  infixl 7 :*:


--eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
