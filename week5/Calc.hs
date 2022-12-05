{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

main :: IO ()
main = print "hi!"


-- 1. Evaluate an expression.
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)


-- 2. With expression parsing.
evalStr :: String -> Maybe Integer
evalStr s = fmap eval maybeExp where
  maybeExp = parseExp Lit Add Mul s


-- 3. Type-class for expressions.
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2


-- 4. More type-class instances.
instance Expr Integer where
  lit n = n
  add n m = n + m
  mul n m = n * m

instance Expr Bool where
  lit n
    | n > 0     = True
    | otherwise = False
  add x y       = x || y
  mul x y       = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y)
    | x > y = MinMax x
    | otherwise = MinMax y
  mul (MinMax x) (MinMax y)
    | x < y = MinMax x
    | otherwise = MinMax y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (mod x 7)
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

