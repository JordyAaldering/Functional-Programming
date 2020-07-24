
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
>
> module Satellite
> where

Satellite data.

> infix 1 :-
> data With a b  =  a :- b
>   deriving (Show)
>
> satellite ∷ With a b -> b
> satellite (_ :- b)  =  b
>
> instance (Eq a) => Eq (With a b) where
>   (a :- _) == (b :- _)  =  a == b
> instance (Ord a) => Ord (With a b) where
>   (a :- _) <= (b :- _)  =  a <= b

