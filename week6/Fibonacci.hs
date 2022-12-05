{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main where 

main :: IO ()
main = print (fib 8)

-- 1. Literal translation of definition into recursive function.
-- Return the n-th Fibonacci number.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib $ n-1) + (fib $ n-2)

-- Naiive list of fibs.
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- 2. First n elements in O(n) time.
fibs2 :: [Integer]
fibs2 = 0 : 1 : (map (\xs -> (fst xs) + (snd xs)) (zip fibs2 (tail fibs2)))

-- 3. Streams
-- Like a list, but without an empty.
-- Note that you can have an infix type constructor so long as its symbol
-- starts with ':'.
infixr 5 :~
data Stream a = a :~ (Stream a)

instance Show a => Show (Stream a) where 
  show xs = show (take 32 (streamToList xs)) ++ "..."


streamToList :: Stream a -> [a]
streamToList (x :~ xs) = x : (streamToList xs)

-- 4. Enriching streams.
streamRepeat :: a -> Stream a
streamRepeat x = x :~ (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :~ xs) = (f x) :~ (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x :~ (streamFromSeed f (f x))

-- 5. Creating some streams.
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- the nth element is the highest power of 2 that divides n.
ruler :: Stream Integer
ruler = level 0 where
  level n = (streamRepeat n) ~~ (level $ n+1)

-- Interleave two streams. abc.. ~~ 123.. becomes a1b2c3.. 
(~~) :: Stream a -> Stream a -> Stream a
(~~) (x :~ xs) ys = x :~ (ys ~~ xs)


-- 6. Fibonacci via Generating Functions.
x :: Stream Integer
-- x = Stream 0 (Stream 1 (streamRepeat 0))
x = 0 :~ 1 :~ (streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = n :~ (streamRepeat 0)
  negate s = streamMap negate s
  (+) (a :~ as) (b :~ bs) = (a+b) :~ (as + bs)
  (*) (a :~ as) (b :~ bs) = (a*b) :~ (streamMap (*a) bs) + (as * (b :~ bs))

instance Fractional (Stream Integer) where
  (/) (a :~ as) (b :~ bs) = q where
    q = (a `div` b) :~ (streamMap (`div` b) (as - q*bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

