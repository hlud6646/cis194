module JoinListBufTest where

import JoinList
import JoinListBuffer

msg = [ "This buffer is for notes you don't want to save, and for"
           , "evaluation of steam valve coefficients."
           , "To load a different file, type the character L followed"
           , "by the name of the file."
           ]
b = fromLines msg