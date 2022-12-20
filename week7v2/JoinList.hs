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
tag Single m _   = m
tag Append m _ _ = m

-- If the annotation represents the number of elements in a tree then we have 
-- an index. If we are after the i-th element, see if the left tree contains 
-- at least that many elements. It it does then recurse on that subtree. Other
-- -wise zoom in on the right tree.
indexJ :: (Sized b, Monoid b) => Int -> JoinlList b a -> Maybe a
indexJ 0 (Single _ a)     = Just a
indexj i Append a l r
  | i <= getSize $ size l = indexJ i l
  | otherwise             = indexJ (a - getSize $ size l) r
indexj _ _                = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append a l r) 
  | n <= getSize $ size l = (dropJ n l) +++ r
  | otherwise             = dropJ (n - getSize $ size l) r
dropJ _ _                 = Empty

