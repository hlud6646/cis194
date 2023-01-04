{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad

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
dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

-- Count the number of elements in a list that satisfy a predicate.
count :: (a -> Bool) -> [a] -> Int
count p xs = sum $ map (fromEnum . p) xs

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a b) = do
  d1             <- die
  d2             <- die
  attack_rolls   <- dice $ max 0 (min 4 a - 1)
  defend_rolls   <- dice $ max 0 (min b 2)
  let pairs       = (reverse . sort $ attack_rolls) `zip` (reverse . sort $ defend_rolls)
      attack_wins = count (uncurry (>)) pairs
      defend_wins = length pairs - attack_wins
  return (Battlefield (a - defend_wins) (b - attack_wins))

invade :: Battlefield -> Rand StdGen Battlefield
invade x@(Battlefield _ 0) = return x
invade x@(Battlefield a _)
    | a < 2 = return x
invade x = battle x >>= invade

-- Was an invasion sucessful?
success :: Battlefield -> Double
success (Battlefield _ defenders) = if defenders == 0 then 1 else 0

successProb :: Battlefield -> Rand StdGen Double
successProb b =  do
  x    <- replicateM 10000 (invade b)
  let y = map success x
  return (sum y / 10000)