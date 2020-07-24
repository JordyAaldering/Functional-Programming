
We did not get to finish this week's assignment entirely
because it was extremely hard and we felt like the
tutorial session did not help us a lot. So we did as
much as we could, but we were having a lot of trouble.
Here goes nothing. :|

Exercise 13.1
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE LambdaCase #-}
> module Foldr
> where

> data List elem list = Nil | Cons elem list

> fold :: (List elem ans -> ans) -> ([elem] -> ans)
> fold alg = consume
>   where consume []     = alg Nil
>         consume (x:xs) = alg (Cons x (consume xs))

> -- Exercise 13.1.1
> foldr' :: (elem -> ans -> ans) -> ans -> [elem] -> ans
> foldr' f x es = fold (\case
> 		Nil -> x
> 		Cons y ys -> f y ys) es

> -- Exercise 13.1.2
> fold' :: (List elem ans -> ans) -> [elem] -> ans
> fold' f es = foldr (\x -> f . Cons x) (f Nil) es

