
Exercise 7.3
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> {-# LANGUAGE UnicodeSyntax #-}
> module Exercise7_3
> where

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

Exercise 7.3.1
    inorderCat (Node l a r) xs
   =	{definition of inorderCat}
    inorder (Node l a r) ++ xs
   =	{definition of inorder}
    inorder l ++ [a] ++ inorder r ++ xs
   =	{definition of inorderCat}
    inorder l ++ [a] ++ inorderCat r xs
   =	{definition of ++}
    inorder l ++ a:(inorderCat r xs)
   =	{definition of inorderCat}
    inorderCat l (a:(inorderCat r xs))

> inorder :: Tree elem -> [elem]
> inorder Empty        = []
> inorder (Node l a r) = inorder l ++ [a] ++ inorder r

> inorderCat :: Tree elem -> [elem] -> [elem]
> inorderCat Empty xs        = xs
> inorderCat (Node l a r) xs = inorderCat l (a:(inorderCat r xs))


Exercise 7.3.2
The new implementation of inorder is indeed more efficient.

> skewed :: Integer -> Tree Integer
> skewed 0 = Empty
> skewed a = Node (skewed (a-1)) a Empty


Exercise 7.3.3

> preorder, postorder :: Tree elem -> [elem] -> [elem]
> preorder  Empty xs        = xs
> preorder  (Node l x r) xs = x : (preorder l (preorder r xs))
> postorder Empty xs        = xs
> postorder (Node l x r) xs = postorder l (postorder r (x:xs))


Exercise 7.3.4
The relation is that there is always a base case and inductive case.
Also when proving we always need an induction assumption.


Exercise 7.3.5
Hugh's lists also use a base case,
but it uses an assosiative function instead of an inductive case.

