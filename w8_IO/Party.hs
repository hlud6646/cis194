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
nextLevel boss pairs = (withBoss, withoutBoss) where
    withBoss    = glCons boss $ mconcat (map snd pairs)   -- Don't bring any of this bosses direct subordinates.
    withoutBoss = mconcat  $ map (uncurry moreFun) pairs  -- For each subdepartment, choose the most fun list since this boss isn't coming anyway.

maxFun :: Tree Employee -> GuestList
maxFun boss = uncurry max (treeFold (mempty, mempty) nextLevel boss)

-- Extractors for GuestList
totalFun :: GuestList -> Fun
totalFun (GL _ n) = n
empNames :: GuestList -> [String]
empNames (GL es _) = sort $ map empName es

display :: GuestList -> IO ()
display gl = do 
    print $ totalFun gl
    mapM_ putStrLn (empNames gl)

main = do
    inpStr <- readFile "company.txt"
    let company = read inpStr :: Tree Employee
    display $ maxFun company