module AST where

-- this template uses prefix constructors; feel free to use ASTInfix.hs (which uses infix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  deriving (Show)

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Maybe a 
eval (Lit k) _ = Just (fromInteger k) 
eval (Add x y) vars = case (eval x vars, eval y vars) of 
                     (Just x', Just y') -> Just (x'+y')
                     _ -> Nothing
                       
eval (Sub x y) vars = case (eval x vars, eval y vars) of 
                     (Just x', Just y') -> Just (x'-y')
                     _ -> Nothing

eval (Mul x y) vars = case (eval x vars, eval y vars) of
                     (Just x', Just y') -> Just (x'*y') 
                     _ -> Nothing 

eval (Div x y) vars = case (eval x vars, eval y vars) of 
                     (Just _, Just 0)  -> Nothing
                     (Just x', Just y') -> Just (x'/y') 
                     _ -> Nothing

eval (Var name) vars = error "FIXME: variable support"
