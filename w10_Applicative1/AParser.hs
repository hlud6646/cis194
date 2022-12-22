module AParser where

import           Control.Applicative

import           Data.Char

import           Data.Tuple.Extra (first)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
instance Functor Parser where
    fmap f (Parser g) = Parser h where
        h s = fmap (first f) (g s)

instance Applicative Parser where
    pure a = Parser f where f s = Just (a, s)
    Parser f <*> Parser h = Parser g where
        g s = case (f s) of
            Just (j, rest) -> fmap (first j) (h rest)
            Nothing        -> Nothing

-- Examples.

-- Look for an 'a', then immediately a 'b'.
abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

-- In stead of keeping the matched part and the rest, return () and the rest.
consume :: Parser a -> Parser ()
consume p = pure (const ()) <*> p

-- Give an () if a then immediately b.
abParser_ :: Parser ()
abParser_ = consume abParser

takeSpace :: Parser [a]
takeSpace = pure(const []) <*> char ' '

threeToOne :: [a] -> [a] -> [a] -> [a]
threeToOne a b c = a ++ b ++ c

-- Extract an Integer and wrap it in a list.
wrappedInt :: Parser [Integer]
wrappedInt = (:) <$> posInt <*> pure ([])

intPair :: Parser [Integer]
intPair = threeToOne <$> wrappedInt <*> takeSpace <*> wrappedInt

instance Alternative Parser where 
  empty = Parser f where f = const Nothing
  (<|>) (Parser f) (Parser g) = Parser h where
    h s = case f s of 
      Nothing -> g s
      x@_     -> x

consumeInt   = consume posInt
consumeUpper = consume (satisfy isUpper)

intOrUppercase :: Parser ()
intOrUppercase = consumeInt <|> consumeUpper