module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

-- Extractor for the annotation.
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

instance (Sized m, Monoid m) => Sized (JoinList m a) where
  size = size . tag

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- |Safely find the 'JoinList' element at the given index.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ value)    = Just value
indexJ idx (Append _ l r)
  | idx < getSize (size l) = indexJ idx l
  | otherwise                = indexJ (idx - getSize (size l)) r
indexJ _ _                   = Nothing


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n x | n < 1 = x
dropJ n (Append _ l r)
  | n > getSize (size l) = dropJ (n - getSize (size l)) r
  | otherwise             = dropJ n l +++ r
dropJ _ _                 = Empty


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Append _ l r)
  | n <= getSize (size l)  = takeJ n l
  | otherwise              = l +++ takeJ (n - getSize (size l)) r
takeJ n _ | n < 1          = Empty
takeJ _ x = x