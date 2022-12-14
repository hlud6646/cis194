import Sized

data JoinList m a  = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- 1. Basic operations on JoinLists.

-- |Create an append of two 'JoinLists' whose monoidal annotation is derived
-- from the operands.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append ((tag x) <> (tag y)) x y

-- |Helper method for '(+++)' which gets the annotation at the root of a 'JoinList'
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single x _)   = x
tag (Append x l r) = (tag l) <> (tag r)


-- 2. Annotation for indexing a JoinList.  Annotation is the number 
-- of elements in a subtree. This allows an ordering of the elements.

instance (Sized m, Monoid m) => Sized (JoinList m a) where
  -- size :: (JoinList m a) -> Size
  size = size . tag

-- |Safely find the 'JoinList' element at the given index.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ value)    = Just value
indexJ idx (Append _ l r)
  | idx < (getSize $ size l) = indexJ idx l
  | otherwise                = indexJ (idx - (getSize $ size l)) r
indexJ _ _                   = Nothing

-- |Drop the first 'n' elements of a 'JoinList'
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append _ l r)
  | n > (getSize $ size l) = dropJ (n - (getSize $ size l)) r
  | otherwise              = (dropJ n l) +++ r
dropJ _ _                  = Empty

-- |Take the firt 'n' elements of a 'JoinList'
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Append _ l r)
  | n-1 < (getSize $ size l) = takeJ n l
  | otherwise              = l +++ (takeJ (n - (getSize $ size l)) r)
takeJ n _ | n < 1 = Empty
takeJ _ x = x







-- |Safely get the element in a list a given index.
(!!?) :: [a] -> Int -> Maybe a
[]       !!? _         = Nothing
_        !!? i | i < 0 = Nothing
(x : xs) !!? 0         = Just x
(x : xs) !!? i         = xs !!? (i-1)

-- |Convert a 'JoinList' to a regular 'List', discarding annotations.
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

hi    = Single 1 "Hi"    :: JoinList Size String
there = Single 1 "there" :: JoinList Size String
how   = Single 1 "how"   :: JoinList Size String
are   = Single 1 "are"   :: JoinList Size String
you   = Single 1 "you"   :: JoinList Size String

jl = (hi +++ there) +++ how +++ (are +++ you)
