{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

-- Roll n dice.
dice :: Integer -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

------------------------------------------------------------
-- Risk

type Army = Int

data Victor = AVictor | BVictor
pairToVictor :: (Integer, Integer) -> Victor
pairToVictor (x, y)
    | x > y     = AVictor
    | otherwise = BVictor

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

count :: a -> [a] -> Integer
count a as = sum $ map (fromEnum . == a) as

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a b) = do
    arolls <- dice $ (min a 3) - 1
    brolls <- dice $  min b 2
    pairs = (sort arolls) `zip` (sort brolls)
    outcomes = map pairToVictor pairs
    Battlefield (a - count BVictor outcomes, b - count AVictor outcomes)


