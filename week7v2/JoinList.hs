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

indexJ :: (Sized b, Monoid b) => Int -> JoinlList b a -> Maybe a
indexJ 1 x@Single _ _ = x
indexj i Append a l r
  | i <= tag l = indexJ i l
  | otherwise = indexJ (a - tag l) r
indexj _ _ = Nothing
