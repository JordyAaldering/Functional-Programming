
Exercise 11.3
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> module Generate
> where

> bools :: [Bool]
> bools = pure False ++ pure True

> maybes :: [elem] -> [Maybe elem]
> maybes elems = pure Nothing ++ (pure Just <*> elems)

> data Suit = Spades | Hearts | Diamonds | Clubs
>   deriving (Show)

> data Rank = Faceless Integer | Jack | Queen | King | Ace
>   deriving (Show)

> data Card = Card Rank Suit | Joker
>   deriving (Show)

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)


> -- Exercise 11.3.1 --
> suit :: [Suit]
> suit = pure Spades ++ pure Hearts ++ pure Diamonds ++ pure Clubs

> rank :: [Rank]
> rank = (pure Faceless <*> [2..10]) ++ pure Jack ++ pure Queen ++ pure King ++ pure Ace

> card :: [Card]
> card = (pure Card <*> rank <*> suit) ++ pure Joker


> -- Exercise 11.3.2 --
> lists :: [a] -> Int -> [[a]]
> lists _ 0 = pure []
> lists x i = pure (:) <*> x <*> lists x (i-1)

> trees :: [a] -> Int -> [Tree a]
> trees _ 0 = (pure Empty)
> trees x 1 = (pure Node <*> pure Empty <*> x <*> pure Empty)
> trees x i = (pure Node <*> trees x (i-1) <*> x <*> pure Empty) ++ (pure Node <*> pure Empty <*> x <*> trees x (i-1))

lists bools 1
[[False], [True]]
lists bools 2
[[False, False], [False, True], [True, False], [True, True]]
trees (lists bools 2) 1
[Node Empty [False,False] Empty,
 Node Empty [False,True]  Empty,
 Node Empty [True, False] Empty,
 Node Empty [True, True]  Empty]
trees (lists bools 2) 2
[Node (Node Empty [False,False] Empty) [False,False] Empty,
 Node (Node Empty [False,False] Empty) [False,True]  Empty,
 Node (Node Empty [False,False] Empty) [True, False] Empty,
 Node (Node Empty [False,False] Empty) [True, True]  Empty,
 Node (Node Empty [False,True]  Empty) [False,False] Empty,
 Node (Node Empty [False,True]  Empty) [False,True]  Empty,
 Node (Node Empty [False,True]  Empty) [True, False] Empty,
 Node (Node Empty [False,True]  Empty) [True, True]  Empty,
 Node (Node Empty [True, False] Empty) [False,False] Empty,
 Node (Node Empty [True, False] Empty) [False,True]  Empty,
 Node (Node Empty [True, False] Empty) [True, False] Empty,
 Node (Node Empty [True, False] Empty) [True, True]  Empty,
 Node (Node Empty [True, True]  Empty) [False,False] Empty,
 Node (Node Empty [True, True]  Empty) [False,True]  Empty,
 Node (Node Empty [True, True]  Empty) [True, False] Empty,
 Node (Node Empty [True, True]  Empty) [True, True]  Empty,
 Node Empty [False,False] (Node Empty [False,False] Empty),
 Node Empty [False,False] (Node Empty [False,True]  Empty),
 Node Empty [False,False] (Node Empty [True, False] Empty),
 Node Empty [False,False] (Node Empty [True, True]  Empty),
 Node Empty [False,True]  (Node Empty [False,False] Empty),
 Node Empty [False,True]  (Node Empty [False,True]  Empty),
 Node Empty [False,True]  (Node Empty [True, False] Empty),
 Node Empty [False,True]  (Node Empty [True, True]  Empty),
 Node Empty [True, False] (Node Empty [False,False] Empty),
 Node Empty [True, False] (Node Empty [False,True]  Empty),
 Node Empty [True, False] (Node Empty [True, False] Empty),
 Node Empty [True, False] (Node Empty [True, True]  Empty),
 Node Empty [True, True]  (Node Empty [False,False] Empty),
 Node Empty [True, True]  (Node Empty [False,True]  Empty),
 Node Empty [True, True]  (Node Empty [True, False] Empty),
 Node Empty [True, True]  (Node Empty [True, True]  Empty)]

