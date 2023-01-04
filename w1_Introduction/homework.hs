{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.Char

main:: IO ()
main = print . length $ h4 15 "a" "b" "s" "t"

-- 1. Separate a long number into its digits.
-- 	  Also define a method which returns the same but reversed.
toDigits :: Integer -> [Integer]
toDigits n
  | n > 0     = map (toInteger . digitToInt) (show n) 
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- 2. Double every second digit starting from the right.
--    It's easier to match from the start of a list so 
--    reverse, do the work and reverse again.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . f . reverse where 
  f []           = []
  f [x]          = [x]
  f (x : y : zs) = x : 2*y : f zs

-- 3. Sum the digits in a list of numbers.
--    Can the version with bind be written purely as a composition
--    as in the other version? (i.e. without reference to xs)
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (xs >>= toDigits)
-- sumDigits = sum . concatMap toDigits

-- 4. Validate a credit card number.
validate :: Integer -> Bool
validate n = remainder == 0 where 
  remainder = rem (sumDigits . doubleEveryOther . toDigits $ n) 10

-- 5. The Towers of Hanoi. Return a list of moves to move n disks
--    from the first peg to the third.
--    n: Number of pegs to move. 
--    a, b, c: source, destination, auxilliary.
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a

-- 6. Towers of Hanoi with four pegs in stead of three.
--    For pegs labelled a, b, s, t where we want to move all from a to b
--    the strategy will be to first move half to s and half to t. This 
--    ratio will then be tweaked.
h4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
h4 0 _ _ _ _ = [] 
h4 1 a b _ _ = [(a, b)]
h4 n a b s t = hanoi top a s t ++ hanoi bot a b t ++ hanoi top s b t where
  top = div n 2
  bot = n - top