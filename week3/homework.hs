module Main where 

-- import Data.List
import Data.List (groupBy, sort, sortBy, tails, maximumBy)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import qualified Data.Map as M

main :: IO ()

x = [1,4,5,4,6,6,3,4,2,4,9]
main = putStr (worker . flipfreqs . freqs $ x)

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




p :: [Integer] -> Bool
p (a : b : c : _) = a < b && b > c
p _ = False

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (!!1) (filter p (map (take 3) (tails xs)))












type Freq = Integer
type Digit = Integer

-- Frequency of digits in the input.
freqs :: [Digit] -> M.Map Digit Freq
freqs xs = M.unionsWith (+) (map (\x -> M.fromList [(x, 1)]) xs)

-- Flip it so that the frequency is the key, to a list of digits with that freq.
flipfreqs :: M.Map Digit Freq -> M.Map Freq [Digit]
flipfreqs m = M.unionsWith (++) maps where
  maps = map (\x -> M.fromList [(fst x, [snd x])]) (map swap (M.toList m))

-- Make a string for the the top line of the histogram.
line :: M.Map Freq [Digit] -> String
line m = (stars . snd . (maximumBy (comparing fst)) . M.toList $ m)

-- Print 10 characters, a space or a star depending on membership in xs
stars :: [Digit] -> String
stars xs = foldr (++) "" (map p [0..9]) where
  p n 
    | elem n xs = "*"
    | otherwise = " "

-- Main recursive method. Make a string for the digit(s) with the highest frequency, 
-- decrement that digit/those digits and repeat.
worker :: M.Map Freq [Digit] -> String
worker m 
  | null m = "==========\n0123456789\n"
  | otherwise =  (line m) ++ "\n" ++ (worker . decrementMax $ m) where 
    decrementMax m = case (M.findMax m) of
                        (1, _)    -> M.deleteMax m
                        (n, digs) -> M.insertWith (++) (n-1) digs (M.deleteMax m)


histogram :: [Integer] -> String
histogram xs = worker . flipfreqs . freqs $ xs
