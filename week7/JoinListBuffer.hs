{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module JoinListBuffer where

import Data.Monoid

import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList (Score, Size) String)  where

  toString = unwords . jlToList

  fromString s =  foldr (+++) Empty (map scoreLine (lines s))
  
  line = indexJ

  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n+1) b

  numLines = getSize . size

  value = getSize . size