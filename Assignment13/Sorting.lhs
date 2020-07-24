
Exercise 13.3
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> module Sorting
> where
> import Unicode
> import Squiggol

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

> data TREE elem tree = EMPTY | NODE tree elem tree

> instance Functor (TREE elem) where
>   fmap _ (EMPTY)      = EMPTY
>   fmap f (NODE l a r) = NODE (f l) a (f r)

> instance Base (TREE elem) where
>   type Rec (TREE elem) = Tree elem
>   inn (EMPTY)          =  Empty
>   inn (NODE l a r)     =  Node l a r
>   out (Empty)          =  EMPTY
>   out (Node l a r)     =  NODE l a r

> grow1, grow2 :: (Ord elem) => [elem] -> Tree elem
> grow1 = unfold (para (fmap (id ▿ inn) ∘ sprout))
> grow2 = fold   (apo  (sprout ∘ fmap (id ▵ out)))

> -- Exercise 13.3.1
> sprout :: (Ord a) => List a (x × TREE a x) -> TREE a (x + List a x)
> sprout Nil = EMPTY
> sprout (Cons _ _) = EMPTY -- We don't know how to do this

> flatten1, flatten2 :: (Ord elem) => Tree elem -> [elem]
> flatten1 = fold   (apo  (wither ∘ fmap (id ▵ out)))
> flatten2 = unfold (para (fmap (id ▿ inn) ∘ wither))

> -- Exercise 13.3.2
> wither :: (Ord a) => TREE a (x × List a x) -> List a (x + TREE a x)
> wither EMPTY = Nil
> wither (NODE _ _ _) = Nil -- We don't know how to do this

> -- Exercise 13.3.3
> sort1, sort2, sort3, sort4 :: Ord elem => [elem] -> [elem]
> sort1 = flatten1 . grow1
> sort2 = flatten1 . grow2
> sort3 = flatten2 . grow1
> sort4 = flatten2 . grow2

