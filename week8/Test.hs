module Test where

import Party
import Data.Tree

t = Node 1 [Node 2 [], Node 3 [ Node 4 []], Node 5 []]


b :: Integer
b = 0
f ::  Integer -> [Integer] -> Integer
f a bs = sum (a : bs)
