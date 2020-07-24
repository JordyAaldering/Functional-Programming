
Exercise 9.2
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
> module Chain
> where
> import Unicode
> import Satellite
> import Tree

> type Cost = Integer
> type Dim  = (Integer, Integer)

> (×) :: Dim -> Dim -> With Cost Dim
> (i, j) × (j', k)
>   | j == j'   = (i * j * k) :- (i, k)
>   | otherwise = error "(×): dimensions do not match"

> (<×>) ∷ With Cost Dim -> With Cost Dim -> With Cost Dim
> (c1 :- d1) <×> (c2 :- d2)
>   = (c1 + c + c2) :- d where c :- d =  d1 × d2

> minCost ∷ [Dim] -> With Cost Dim
> minCost [a] = 0 :- a
> minCost as  = minimum [ minCost bs <×> minCost cs | (bs, cs) ← split as ]

> split ∷ [a] -> [([a], [a])]
> split []       = error "split: empty list"
> split [_a]     = []
> split (a : as) = ([a], as) : [ (a : bs, cs) | (bs, cs) ← split as]

minCost [(10, 30), (30, 5), (5, 60)]
minCost [(i, i + 1) | i <- [1 .. 3]]
minCost [(i, i + 1) | i <- [1 .. 9]]


Exercise 9.2.1

> minimumCost :: (size -> size -> With Cost size) -> [size] -> With Cost size
> minimumCost _ [a]   = 0 :- a
> minimumCost cost as = minimum [minimumCost cost bs `combine` minimumCost cost cs | (bs, cs) <- split as]
>   where combine (c1 :- s1) (c2 :- s2) = let (c :- s) = cost s1 s2 in (c1 + c2 + c) :- s


Exercise 9.2.2a

> concatCost :: Integer -> Integer -> With Cost Integer
> concatCost x y = x :- x + y

Exercise 9.2.2b
The cost is proportional to the maximum of the argument sizes
because it is impossible to do this in an exact way.
Therefor we make a worst case estimation.

> additionCost :: Integer -> Integer -> With Cost Integer
> additionCost x y = max x y :- 1 + max x y


Exercise 9.2.3

> optimalChain :: (size -> size -> With Cost size) -> [size] -> With Cost (With size (Tree size))
> optimalChain _ [a]   = 0 :- (a :- Leaf a)
> optimalChain cost as = minimum [optimalChain cost bs `combine` optimalChain cost cs | (bs, cs) <- split as]
>   where combine (c1 :- (s1 :- t1)) (c2 :- (s2 :- t2)) = let (c :- s) = cost s1 s2 in (c1 + c2 + c) :- (s :- (t1 :^: t2))


Exercise 9.2.4
When ++ is left-associative the composition operation may need to be reduced more times,
because a function earlier in the chain can shortcut such that it does not need the result
of a nested computation. In the worst case scenarios, left- and right-associativity are
the same, but in other cases right-associativity can be a win.
