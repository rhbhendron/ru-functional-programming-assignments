module ASTInfix where
import Result
import Control.Applicative (Applicative(liftA2))

-- this template uses infix constructors; feel free to use AST.hs (which uses prefix ones) if you prefer
-- (if you really liked your own solution to Exercise 4.7, you can use that as well)

type Identifier = String

data Expr = Lit Integer | Var Identifier | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr
  deriving (Show)

infixl 6 :+: 
infixl 6 :-: 
infixl 7 :/:
infixl 7 :*:

evalFunc :: (Fractional a, Eq a,Applicative m) => Expr -> Expr -> [(Identifier, a)] -> (a -> a -> a) -> (Expr -> [(Identifier, a)] -> m a) -> m a
evalFunc x y vars f evalF= f <$> evalF x vars <*> evalF y vars

eval :: (Fractional a, Eq a) => Expr -> [(Identifier,a)] -> Result a
eval (Lit k) _ = Okay (fromInteger k)
eval (x :+: y) vars = evalFunc x y vars (+) eval
eval (x :-: y) vars = evalFunc x y vars (-) eval
eval (x :*: y) vars = evalFunc x y vars (/) eval
eval (x :/: y) vars = (/) <$> eval x vars <*> evalY where
  evalY
   | eval y vars == Okay 0 = Error ["divide by zero"]
   | otherwise = eval y vars
eval (Var name) vars = case lookup name vars of
                         Just x -> Okay x
                         Nothing -> Error ["unknown variable: " ++ name]

evalM :: (Fractional a, Eq a) => Expr -> [(Identifier, a)] -> Maybe a
evalM (Lit k) _ = Just (fromInteger k)
evalM (x :+: y) vars = evalFunc x y vars (+) evalM
evalM (x :-: y) vars = evalFunc x y vars (-) evalM
evalM (x :*: y) vars = evalFunc x y vars (*) evalM
evalM (x :/: y) vars = (/) <$> evalM x vars <*> evalY where
  evalY
    | evalM y vars == Just 0 = Nothing
    | otherwise = evalM y vars
evalM (Var name) vars = lookup name vars
