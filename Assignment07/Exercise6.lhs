
Exercise 7.6
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> {-# LANGUAGE UnicodeSyntax #-}
> module Exercise7_6
> where


Exercise 7.6.1
maxSegmentProduct :: [Integer] -> Integer
maxSegmentProduct = maximum . map product . segments

segments :: [a] -> [[a]]
segments = concat . map inits . tails

    maximum . map sum . segments
   =	{definition of segments}
    maximum . map sum . concat . map inits . tails
   =	{polymorphism of concat}
    maximum . concat . map (map sum) . map inits . tails
   =	{bookkeeping law}
    maximum . map maximum . map (map sum) . map inits . tails
   =	{map-rule}
    maximum . map (maximum . map sum) . map inits . tails
   =	{map-rule}
    maximum . map (maximum . map sum . inits) . tails

> maxSegmentProduct :: [Integer] -> Integer
> maxSegmentProduct = maximum . scanr (\a b -> 0 ^ a * b) 0

Exercise 7.6.2
?

Exercise 7.6.3
The transformation is really necessary.
The original problem could be specified with the maxProfit
function below.

> maxProfit :: [Integer] -> Integer
> maxProfit = maximum . scanr (\a b -> 0 ^ a + b) 0

Exercise 7.6.4
Multiple, non-overlapping, transactions could be allowed
by applying the maxProfit function multiple times to find
more than one transactions.

