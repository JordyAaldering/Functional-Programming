
Exercise 13.2
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE DeriveFunctor #-}
> module Minimax2
> where
> import Squiggol

> type Position = (Int, Int)

> data Tree e = Node e [Tree e]
> data TreeSlice e xs = Slice e [xs]

> -- Exercise 13.2.1
> instance Functor (TreeSlice e) where
>   fmap f (Slice e xs) = Slice e (f <$> xs)

> instance Base (TreeSlice e) where
>   type Rec (TreeSlice e) = Tree e
>   inn (Slice e xs) = Node  e xs
>   out (Node  e xs) = Slice e xs

> -- Exercise 13.2.2
> size, depth :: Tree elem -> Integer
> size  e = fold (\(Slice _ xs) -> sum     xs + 1) e
> depth e = fold (\(Slice _ xs) -> maximum xs + 1) e

> -- Exercise 13.2.3
> gametree :: (position -> [position]) -> (position -> Tree position)
> gametree f = unfold (\p -> Slice p (f p))

> -- Exercise 13.2.4
> winning :: Tree position -> Bool
> winning p = fold (\(Slice _ xs) -> any not xs) p

