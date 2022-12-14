module Scrabble where

import Data.List (find)
import Data.Maybe (fromMaybe)

scoreData :: [(String, Score)]
scoreData = [("aeilnordstu", Score 1),
             ("dg", Score 2),
             ("bcmp", Score 3),
             ("fhvwy", Score 4),
             ("k", Score 5),
             ("xj", Score 8),
             ("qz", Score 10)]

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c = fromMaybe 0 (snd <$> x) where
  x = find (\(chars, value) -> elem c chars) scoreData
