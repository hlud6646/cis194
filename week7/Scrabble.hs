module Scrabble

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

("aeilnordstu", 1), ("dg", 2), ("bcmp", 3)

score :: Char -> Score

