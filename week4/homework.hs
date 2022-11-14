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
fun1 = product . filter (even) . map (\x -> x - 2)

fun2 :: Integer -> Integer 
-- fun2 1 = 0
-- fun2 n 
--   | even n = n + fun2 (n ‘div‘ 2) 
  -- | otherwise = fun2 (3 * n + 1)
fun2 = sum . filter (even) . iterate collatz where 
  collatz n
    | even n    = div n 2
    | otherwise = 3 * n + 1


-- 2. Folding Trees.
-- TODO; need to implement a recursive AVL tree :/
-- This course is heavy.
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Make a tree from a list of elements.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a 
insert _ _ = Leaf


-- 3. More folds.
-- Exclusive or: True iff there are an odd number of Trues.
xor :: [Bool] -> Bool
xor xs = foldr (\_ acc -> not acc) False (filter (\x -> x) xs)

-- Map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f as = foldr (\a acc -> (f a) : acc) [] as

-- foldl as foldr
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f zero as = foldr (\x acc -> f acc x) zero as


-- 4. Sieve of Sundaram. 
-- This is a little hacky but 
sundaram :: Integer -> [Integer]
sundaram n = map (\d -> d * 2 + 1 ) preprimes where
  candidates = [i + j + 2*i*j | j <- [1..(div (n-1) 2)], i <- [1..j]]   
  out = takeWhile (<n) . sort . takeWhile (<3*n) $ candidates
  preprimes = [x | x <- [1..n], not (elem x out)]
