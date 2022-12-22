module JoinListBufEditor where

import JoinListBuffer
import JoinList
import Editor

startMsg :: [String]
startMsg = [ "This buffer is for notes you don't want to save, and for"
           , "evaluation of steam valve coefficients."
           , "To load a different file, type the character L followed"
           , "by the name of the file."
           ]

main :: IO ()
main = runEditor editor $ fromLines startMsg