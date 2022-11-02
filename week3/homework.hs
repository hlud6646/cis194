
module Main where 

main :: IO ()

x :: [[Int]]
x = skips []
main = print (skips "hello there!")

h = head
d = drop
l = length

nth :: [a] -> Int -> [a]
nth [] _ = []
nth x n 
  | l x > n = ( h (d n x)) : (nth (d (n + 1) x) n)
  | otherwise = []

skips :: [a] -> [[a]]
skips x = map (nth x ) [0..(l x) - 1] 
