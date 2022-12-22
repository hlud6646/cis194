{-# OPTIONS_GHC -Wall #-}

module Main where

import LogAnalysis

main :: IO ()
main = do 
  contents <- readFile "error.log"
  let x = whatWentWrong . inOrder . build . parse $ contents
  mapM_ print x



