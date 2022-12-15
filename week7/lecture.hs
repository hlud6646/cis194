{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
    ins     = map (\(x, y) -> show' x ++ show' y)  b2
    outs    = map (show' . uncurry f) b2

show' :: Bool -> String
show' True  = "T"
show' False = "F"

b  :: [Bool]
b   = [True, False]

b2 :: [(Bool, Bool)]
b2  = [(x, y) | x <- b, y <- b]

b3 :: [(Bool, Bool, Bool)]
b3  = (,,) <$> b <*> b <*> b

b4 :: [(Bool, Bool, Bool, Bool)]
b4 = (,,,) <$> b <*> b <*> b <*> b

-- Check (a . b) . c == a . (b . c) 
checkOne :: BinaryOp -> (Bool, Bool, Bool) -> Bool
checkOne f (x, y, z) = f (f x y) z == f x (f y z)

-- Check whether a given binary operator is associative.
associative :: BinaryOp -> Bool
associative f = all (checkOne f) b3

-- A BinaryOp is fully described by its image, i.e. the values to which 
-- its four inputs are mapped. So from a tuple of four boolans we can
-- easily create an anonymous BinaryOp. 
opFromImg :: (Bool, Bool, Bool, Bool) -> BinaryOp
opFromImg (w, x, y, z) = g where
  g True True   = w
  g True False  = x
  g False True  = y
  g False False = z

-- For example:
and' :: BinaryOp
and' = opFromImg (True, False, False, False)

allBinaryOps :: [BinaryOp]
allBinaryOps = map opFromImg b4

associativeOps :: [BinaryOp]
associativeOps = filter associative allBinaryOps

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
isNeutral :: BinaryOp -> Bool -> Bool
isNeutral f e = all (\x -> f e x == f x e && f x e == x) [True, False]

-- Find a (the) neutral element for an operator if it exists.
neutral :: BinaryOp -> Maybe Bool
neutral f
  | isNeutral f True  = Just True
  | isNeutral f False = Just False
  | otherwise          = Nothing

neutrals :: [Maybe Bool]
neutrals = map neutral associativeOps

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