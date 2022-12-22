module Main where 

-- import Data.List
import Data.List (tails, transpose, maximum)

main :: IO ()

x = [1,2,3,2,4,2,4,2,4]
main = print (localMaxima x)



-- Simply map over the range 1,2,...,length of the iterable
-- using the function defined above partially applied.
skips :: [a] -> [[a]]
skips x = map (nth x ) [0..(length x) - 1] 

-- Given a list and an integer, return every nth element of that list.
-- If the list is empty or if n is greater than the length of the list,
-- then return the empty list. Otherwise drop the first n elements,
-- then take the head and append this function applied to whatever is left.
nth :: [a] -> Int -> [a]
nth [] _ = []
nth x n 
  | length x > n = ( head (drop n x)) : (nth (drop (n + 1) x) n)
  | otherwise = []





-- Return a list of the local maxima in an input list. 
-- Traverse all tails (of length 3 or more) and check if the 
-- first three elemens are [small, big, small].
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (!!1) (filter p (map (take 3) (tails xs)))
  where p (a : b : c : _) = a < b && b > c
        p _ = False






-- Print a histogram given a list of numbers.
histogram :: [Integer] -> String
histogram xs = title . reverse . unlines . transpose . padAll $ (map star (count xs))
  where title s = s ++ "\n==========\n0123456789\n"

count :: [Integer] -> [Int]
count xs = map (\d -> length (filter (==d) xs)) [0..9]

star :: Int -> String
star n = replicate n '*'

pad :: Int-> [Char] -> [Char]
pad m x
  | m == length x = x
  | otherwise = pad m (x ++ " ")

padAll :: [String] -> [String]
padAll xs = map (pad m) xs where
  m = maximum (map length xs)
