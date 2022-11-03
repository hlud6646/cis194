module Main where 

import Data.List

main :: IO ()

x = [1,2,4,4,2,4,6,2,4,57,45,35,2,4,5]
main = print (localMaxima x)

h = head
d = drop
l = length

-- Given a list and an integer, return every nth element of that list.
-- If the list is empty or if n is greater than the length of the list,
-- then return the empty list. Otherwise drop the first n elements,
-- then take the head and append this function applied to whatever is left.
nth :: [a] -> Int -> [a]
nth [] _ = []
nth x n 
  | l x > n = ( h (d n x)) : (nth (d (n + 1) x) n)
  | otherwise = []

-- Simply map over the range 1,2,...,length of the iterable
-- using the function defined above partially applied.
skips :: [a] -> [[a]]
skips x = map (nth x ) [0..(l x) - 1] 




f :: [Integer] -> Bool
f (a : b : c : _) = a < b && b > c
f _ = False

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (!!1) (filter f (map (take 3) (tails xs)))

