{-# OPTIONS_GHC -Wall #-}

module Lecture where 

-- Monoids:
-- A type with an associative binary opertion <> and a two sided identity.
-- In other words, a semi-group with identity.

class Semigroup' a where
  -- This operation is assumed to be associative.
  (<~>) :: a -> a -> a

class Semigroup' a => Monoid' a where
  -- A two sided inverse for the inherited <> function.
  mempty :: a

instance Semigroup' [a] where
  (<~>) = (++)
instance Monoid' [a] where
  mempty = []

-- How many different instances of Monoid Bool are there?
-- 
-- Let B = {0, 1}. Then B^2 contains 4 elements and so there 
-- are 2^4 = 16 functions functions BxB -> B.  We could check 
-- by hand but 16 cases justifies automatic checking.

type BinaryOp = Bool -> Bool -> Bool

instance Show BinaryOp where
  show f = show (foldr (++) "" actions) where
    actions = zipWith (\x y -> concat ["  ", x, " |-> ",y, "  "])  ins outs
    ins     = map (\(x, y) -> show' x ++ show' y)  bools2
    outs    = map show' (map (uncurry f) bools2)

show' :: Bool -> String
show' True  = "T"
show' False = "F"

bools  :: [Bool]
bools   = [True, False]

bools2 :: [(Bool, Bool)]
bools2  = [(x, y) | x <- bools, y <- bools]

bools3 :: [(Bool, Bool, Bool)]
bools3  = [(x, y, z) | x <- bools, y <- bools, z <- bools]

bools4 :: [(Bool, Bool, Bool, Bool)]
bools4 = [(w, x, y, z) | w <- bools, x <- bools, y <- bools, z <- bools]

-- Check (a . b) . c = a . (b . c) 
checkOne :: BinaryOp -> (Bool, Bool, Bool) -> Bool
checkOne f (a, b, c) = f (f a b) c == f a (f b c)

-- Check whether a given binary operator is associative.
associative :: BinaryOp -> Bool
associative f = all (checkOne f) bools3

-- A BinaryOp is fully described by its image, i.e. the values to which 
-- its four inputs are mapped. So from a tuple of four boolans we can
-- easily create an anonymous BinaryOp. 
op_from_img :: (Bool, Bool, Bool, Bool) -> BinaryOp
op_from_img (a, b, c, d) = g where
  g True True   = a
  g True False  = b
  g False True  = c
  g False False = d

-- For example:
and' :: BinaryOp
and' = op_from_img (True, False, False, False)

allBinaryOps :: [BinaryOp]
allBinaryOps = map op_from_img bools4

associative_ops :: [BinaryOp]
associative_ops = filter associative allBinaryOps

-- Here they are.
-- "  TT |-> T    TF |-> T    FT |-> T    FF |-> T  " ... constant True
-- "  TT |-> T    TF |-> T    FT |-> T    FF |-> F  " ... or
-- "  TT |-> T    TF |-> T    FT |-> F    FF |-> F  " ... True in 1st arg
-- "  TT |-> T    TF |-> F    FT |-> T    FF |-> F  " ... True in 2nd arg
-- "  TT |-> T    TF |-> F    FT |-> F    FF |-> T  " ... equal args
-- "  TT |-> T    TF |-> F    FT |-> F    FF |-> F  " ... and
-- "  TT |-> F    TF |-> T    FT |-> T    FF |-> F  " ... exclusive or
-- "  TT |-> F    TF |-> F    FT |-> F    FF |-> F  " ... constant False

-- Now check that one of t/f is neutral.

-- Check if b is neutral for f.
is_neutral :: BinaryOp -> Bool -> Bool
is_neutral f b = and $ map (\x -> f b x == f x b && f x b == x) [True, False]

-- Find a (the) neutral element for an operator if it exists.
neutral :: BinaryOp -> Maybe Bool
neutral f
  | is_neutral f True  = Just True
  | is_neutral f False = Just False
  | otherwise          = Nothing

neutrals :: [Maybe Bool]
neutrals = map neutral associative_ops

-- At long last here they are:
--
-- ("  TT |-> T    TF |-> T    FT |-> T    FF |-> T  ",Nothing)
-- ("  TT |-> T    TF |-> T    FT |-> T    FF |-> F  ",Just False)
-- ("  TT |-> T    TF |-> T    FT |-> F    FF |-> F  ",Nothing)
-- ("  TT |-> T    TF |-> F    FT |-> T    FF |-> F  ",Nothing)
-- ("  TT |-> T    TF |-> F    FT |-> F    FF |-> T  ",Just True)
-- ("  TT |-> T    TF |-> F    FT |-> F    FF |-> F  ",Just True)
-- ("  TT |-> F    TF |-> T    FT |-> T    FF |-> F  ",Just False)
-- ("  TT |-> F    TF |-> F    FT |-> F    FF |-> F  ",Nothing)


