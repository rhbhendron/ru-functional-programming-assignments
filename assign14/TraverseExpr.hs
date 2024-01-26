{-# LANGUAGE InstanceSigs #-}
module TraverseExpr where

import Control.Monad.State -- or import State
import Data.Foldable
import Data.List
import Data.Maybe

data Expr var = Var var | Lit Integer | Op BinOp (Expr var) (Expr var)
  deriving (Show,Eq)
data BinOp    = Add | Sub | Mul | Div
  deriving (Show,Eq)

instance Functor Expr where
  fmap :: (a -> b) -> (Expr a -> Expr b)
  fmap f (Var a) = Var (f a)
  fmap f (Lit x) = Lit x
  fmap f (Op bin x y) = Op bin (fmap f x) (fmap f y)

instance Foldable Expr where
  foldMap :: (Monoid m) => (a -> m) -> Expr a -> m
  foldMap f (Var a) = f a
  foldMap f (Lit x) = mempty
  foldMap f (Op bin x y) = foldMap f x <> foldMap f y

instance Traversable Expr where
  traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (Var a) = Var <$> f a
  traverse f (Lit x) = pure (Lit x)
  traverse f (Op bin x y) = Op bin <$> traverse f x <*> traverse f y

allVars :: (Ord a) => Expr a -> [a]
allVars = nub . foldMap (:[])

--renameVar :: String -> State [(String,Int)] Int
--renameVar name = do ...

--indexVars :: Expr String -> Expr Int
