{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative

import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

atom :: Parser Atom
atom = fmap N posInt <|> fmap I ident

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

openB, closeB :: Parser Char
openB  = char '('
closeB = char ')'

bracketed, strip :: Parser a -> Parser a
bracketed p = openB *> p <* closeB
strip     p = spaces *> p <* spaces

sexpr :: Parser SExpr
sexpr = strip $ (A <$> atom) <|> bracketed (fmap Comb (oneOrMore sexpr))