{-# OPTIONS_GHC -Wall #-}

module Main where 

main :: IO ()
main = print "Hi!"



-- Definining a type with a parameter. It's actually bananas how terse this is.
data List t = Empty | Cons t (List t)

lst1 :: List Int
lst1 = Cons 3 (Cons 5 (Cons 7 Empty))

lst2 :: List String
lst2 = Cons "list" (Cons "two" Empty)

-- And a polymorphic function to go with it.
filter' :: (t -> Bool) -> List t -> List t
filter' _ Empty = Empty
filter' p (Cons x xs)  
  | p x         = Cons x (filter' p xs)
  | otherwise   = filter' p xs

map' :: (t -> s) -> List t -> List s
map' _ Empty = Empty
map' f (Cons x xs) = Cons (f x) (map' f xs)
