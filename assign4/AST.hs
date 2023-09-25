module AST where

--start with either:
--  data Expr = Lit Integer | Add Expr Expr | Mul Expr Expr | ...
--or:
data Expr = Lit Integer | Expr :+: Expr | Expr :*: Expr | Expr :-: Expr | Expr :/: Expr |
                Var
infixl 6 :+:
infixl 7 :*:
infixl 6 :-:
infixl 7 :/:

maybeFunctions :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeFunctions f x y = pure f <*> x <*> y

eval :: (Fractional a, Eq a) => Expr -> a -> Maybe a
eval (Lit x) y = Just $ fromIntegral x
eval (x :+: y) z = maybeFunctions (+) (eval x z) (eval y z)
eval (x :*: y) z = maybeFunctions (*) (eval x z) (eval y z)
eval (x :-: y) z = maybeFunctions (-) (eval x z) (eval y z)
eval (x :/: y) z = if (eval y z) == Just 0.0 then Nothing else maybeFunctions (/) (eval x z) (eval y z)
eval Var z = Just z
