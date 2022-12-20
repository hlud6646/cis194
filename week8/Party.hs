module Party where

import Employee
import Data.Tree

-- Assuming that the new guest is not already in the list.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Semigroup GuestList where
    GL e1 f1 <> GL e2 f2 = GL (e1 ++ e2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: b -> ([b] -> a -> b) -> Tree a -> b
treeFold e f (Node a []) = f [e] a
treeFold e f (Node x as) = f (map (treeFold e f) as) x
