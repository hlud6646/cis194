module Party where

import System.IO

import Employee
import Data.Tree
import Data.List (sort)

-- Assuming that the new guest is not already in the list.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Semigroup GuestList where
    GL e1 f1 <> GL e2 f2 = GL (e1 ++ e2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: b               -- The initial value of the accumulator.
        -> (a -> [b] -> b)  -- How to describe this argument?
        -> Tree a           -- The tree which is folded over.
        -> b                -- Returns the final accumulator.
treeFold e f (Node a tas) =  f a (map (treeFold e f) tas)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss pairs = (bestWithBoss boss pairs, bestWithoutBoss pairs) where
    bestWithBoss lackey []  = glCons lackey mempty                      -- A bottom rung employee goes on a singleton GL.
    bestWithBoss boss pairs = glCons boss (maximum (map snd pairs))     -- Only add Bob to lists which don't contain a direct subordinate.
    bestWithoutBoss []  = mempty                                        -- The employee has no subordinates, but this function insists that they still don't go. So an empty GL.
    bestWithoutBoss pairs   = maximum (map fst pairs ++ map snd pairs)  -- Best list out of everything available. Bob's not coming, so no fun detraction possible.

maxFun :: Tree Employee -> GuestList
maxFun boss = (uncurry max) (treeFold (mempty, mempty) nextLevel boss)

totalFun :: GuestList -> Fun
totalFun (GL _ n) = n
empNames :: GuestList -> [String]
empNames (GL es _) = sort $ map empName es

main = do
    inpStr <- readFile "company.txt"
    let company = read inpStr :: Tree Employee
        gl      = maxFun company
    putStrLn . show $ totalFun gl
    mapM_ putStrLn (empNames gl)