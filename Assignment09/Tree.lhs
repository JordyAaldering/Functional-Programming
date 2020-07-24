
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
>
> module Tree
> where

Leaf trees.

> data Tree elem  =  Leaf elem | Tree elem :^: Tree elem
>   deriving (Show, Eq, Ord)
