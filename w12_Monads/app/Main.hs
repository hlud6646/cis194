module Main where

import Risk

import Control.Monad.Random

main :: IO ()
main = do
    let b = Battlefield 12 12
    outcome <- evalRandIO (successProb b)
    print outcome