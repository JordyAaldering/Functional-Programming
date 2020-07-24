
Exercise 7.2
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> {-# LANGUAGE UnicodeSyntax #-}
> module Exercise7_2
> where

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

Exercise 7.2.1
Base case: P(Empty)
Inductive case: P(left) /\ P(right) => P(Node left elem right)


Exercise 7.2.2
We need to prove that: P(x) = innerNodes x + 1 = outerNodes
Now we need to prove the base case and inductive case.
    Base case:      x = Empty
    Inductive case: x = (Node l e r)

Base case:
    innerNodes Empty + 1
   =	{definition of innerNodes}
    0 + 1
   =	{definition of +}
    1
   =	{definition of outerNodes}
    outerNodes Empty

Inductive case:
Induction assumption: innerNodes l + 1 = outerNodes r

    innerNodes (Node l e r) + 1
   =	{definition of innerNodes}
    innerNodes l + innerNodes r + 1 + 1
   =	{commutativity of +}
    innerNodes l + 1 + innerNodes r + 1
   =	{induction assumption}
    outerNodes l + outerNodes r 
   =	{definition of outerNodes}
    outerNodesEmpty (Node l e r)

> innerNodes, outerNodes :: Tree elem -> Int
> innerNodes Empty        = 0
> innerNodes (Node l _ r) = innerNodes l + innerNodes r + 1
> outerNodes Empty        = 1
> outerNodes (Node l _ r) = outerNodes l + outerNodes r

Exercise 7.2.3
We need to prove that: P(x) := 2^(minHeight x) - 1 <= size x <= 2^(maxHeight x) - 1
Now we need to prove the base case and inductive case.
    Base case:      x = Empty
    Inductive case: x = (Node l e r)

Base case:
    2^(minHeight Empty) - 1 <= size Empty <= 2^(maxHeight Empty) - 1
   =	{definition minHeight}
    2^0 - 1 <= size Empty <= 2^(maxHeight Empty) - 1
   =	{definition size}
    2^0 - 1 <= 0 <= 2^(maxHeight Empty) - 1
   =	{definition maxHeight}
    2^0 - 1 <= 0 <= 2^0 - 1
   =	{definition of ^ and -}
    0 <= 0 <= 0

Inductive case:
Induction assumption: ...

    2^(minHeight (Node l e r)) - 1 <= size (Node l e r) <= 2^(maxHeight (Node l e r)) - 1
   =	{}
    ...

> size, minHeight, maxHeight :: Tree elem -> Int
> size Empty        = 0
> size (Node l _ r) = size l + size r + 1

> minHeight Empty        = 0
> minHeight (Node l _ r) = min (minHeight l) (minHeight r) + 1
> maxHeight Empty        = 0
> maxHeight (Node l _ r) = max (maxHeight l) (maxHeight r) + 1

