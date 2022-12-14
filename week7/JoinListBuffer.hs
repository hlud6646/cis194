{-# LANGUAGE FlexibleInstances, TypeSynonymInstances  #-}
module JoinListBuffer where

import Data.Monoid

import Buffer
import JoinList
import Sized
import Scrabble

instance Buffer (JoinList (Score, Size) String)  where

  toString = unwords . jlToList

  fromString s = map (lines s)

  
