{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinListBuffer where

import Buffer
import Scrabble
import Sized
import JoinList
    ( JoinList(..), (+++), tag, indexJ, dropJ, takeJ, jlToList )
import System.Posix.Internals (lstat)

-- The first argument should be a string representing a single line.
fromLine :: String -> JoinList (Score, Size) String
fromLine s = Single (scoreString s, 1) s

-- helper for `fromString` below used for creating balanced trees.
fromLines :: [String] -> JoinList (Score, Size) String
fromLines []  = Empty
fromLines [l] = fromLine l
fromLines ls  = l +++ r where
    split = length ls `div` 2
    l = fromLines $ take split ls
    r = fromLines $ drop split ls

instance Buffer (JoinList (Score, Size) String) where
    toString  = unwords . jlToList
    fromString = fromLines . lines
    line = indexJ
    replaceLine n s b = left +++ new +++ right where
        left  = takeJ n b
        new   = fromLine s
        right = dropJ (n+1) b
    numLines Empty = 0
    numLines (Single _ _) = 1
    numLines (Append _ l r) = numLines l + numLines r
    value = getScore . fst . tag