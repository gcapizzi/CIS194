module Calc
( eval,
  evalStr,
  lit,
  add,
  mul,
) where

import ExprT
import Parser

evalStr :: String -> Maybe Integer
evalStr x = do
    px <- parseExp lit add mul x :: Maybe ExprT
    return $ eval px

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a
    eval :: a -> Integer

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul
    eval (Lit x) = x
    eval (Add x y) = eval x + eval y
    eval (Mul x y) = eval x * eval y

