{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall  #-}

module VarCalc where

import qualified Data.Map as M

main :: IO ()
main = print "Hi!"

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- A thing of type a is a HasVars if there is a sensible notion 
-- of assigning the thing a name.
class HasVars a where
  var :: String -> a

data VarExprT = Variable String
              | Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              deriving (Show)

instance HasVars VarExprT where
  var s = Variable s

instance Expr VarExprT where
  lit n = Lit n
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

instance HasVars (M.Map String Integer -> Maybe Integer) where
  -- partially apply M.lookup
  var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
  -- the constant function Just n
  lit n = \_ -> Just n
  add f1 f2 = ??? 







