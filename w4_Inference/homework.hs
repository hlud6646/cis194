{-# OPTIONS_GHC -Wall #-}

module Main where 

import Data.List (sort)

main :: IO ()
main = print ( sundaram 100)

-- 1. Reimplement these functions in idiomatic style.
fun1 :: [Integer] -> Integer 
-- fun1 [] = 1
-- fun1 (x:xs)
--      | even x    = (x - 2) * fun1 xs
--      | otherwise = fun1 xs
fun1 = product . filter even . map (\x -> x - 2)

fun2 :: Integer -> Integer 
-- fun2 1 = 0
-- fun2 n 
--   | even n = n + fun2 (n ‘div‘ 2) 
  -- | otherwise = fun2 (3 * n + 1)
fun2 = sum . filter even . iterate collatz where 
  collatz n
    | even n    = div n 2
    | otherwise = 3 * n + 1


-- 2. Folding Trees.
--    The task is to use a fold to build a balanced tree from a 
--    given list. Don't be spooked, you don't need to implement 
--    a full AVL tree since all that is required is a tree that 
--    isn't useful for anything, since there is no context on a
--    requiring it to be ordeered.
--
--    I feel like this would be easier to do without a fold, si
--    -mply splittling a list at its midpoint and making subtre
--    -es of each sublist, then joining at a node...
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Make a tree from a list of elements.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a 
insert x Leaf = Node 1 Leaf Leaf
insert x Node 1 Leaf Leaf = Node 2 (Node 1 Leaf Leaf) Leaf
insert x Node h ()


-- 3. More folds.
-- Exclusive or: True iff there are an odd number of Trues.
xor :: [Bool] -> Bool
xor xs = foldr (\_ acc -> not acc) False (filter id xs)

-- Map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> (f a) : acc) []

-- foldl as foldr
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f zero = foldr (\x acc -> f acc x) zero


-- 4. Sieve of Sundaram. 
-- This is a little hacky but 
sundaram :: Integer -> [Integer]
sundaram n = map (\d -> d * 2 + 1 ) preprimes where
  candidates = [i + j + 2*i*j | j <- [1..(div (n-1) 2)], i <- [1..j]]   
  out = takeWhile (<n) . sort . takeWhile (<3*n) $ candidates
  preprimes = [x | x <- [1..n], x `notElem` out]