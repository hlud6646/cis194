module JoinListTest where

import JoinList
import Sized
import Scrabble


hi, there, how, are, you :: JoinList Size String
hi    = Single 1 "hi"
there = Single 1 "there"
how   = Single 1 "how"
are   = Single 1 "are"
you   = Single 1 "you"

x :: JoinList Size String
x = (hi +++ there +++ how) +++ (are +++ you)

m1 = mapM_ (\n -> print $ jlToList (takeJ n x)) [-2..7]

m2 = mapM_ (\n -> print $ indexJ n x) [-2..7]

scoreLine :: String -> JoinList Score String
scoreLine s = Single  (scoreString s) s

m3 = print ( scoreLine "yay" +++ scoreLine "haskell" )