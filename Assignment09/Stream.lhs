Exercise 9.3
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
> module Stream
> where
> import Prelude hiding (head, tail, repeat, map, zip, take, sum)
> import Unicode

> data Stream elem = Cons {head :: elem, tail :: Stream elem}
>   deriving (Show)

> infixr 5 ≺
> (≺) :: elem -> Stream elem -> Stream elem
> a ≺ s = Cons a s

> from :: Integer -> Stream Integer
> from n = n ≺ from (n + 1)


Exercise 9.3.1

> repeat :: a -> Stream a
> repeat x = x ≺ repeat x

> map :: (a -> b) -> Stream a -> Stream b
> map f x = f (head x) ≺ map f (tail x)

> zip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
> zip f x y = f (head x) (head y) ≺ zip f (tail x) (tail y)


Exercise 9.3.2

> instance (Num elem) => Num (Stream elem) where
>     fromInteger = repeat . fromInteger
>     (+) = zip (+)
>     (*) = zip (*)
>     abs = map (abs)
>     signum = map (signum)
>     negate = map (negate)

> nat, fib :: Stream Integer
> nat = 0 ≺ nat + 1
> fib = 0 ≺ 1 ≺ fib + tail fib


Exercise 9.3.3

> take :: Integer -> Stream elem -> [elem]
> take n xs
>     | n <= 0    = []
>     | otherwise = (head xs) : take (n-1) (tail xs)


Exercise 9.3.4

> diff :: (Num elem) => Stream elem -> Stream elem
> diff s = tail s - s

> sum :: (Num elem) => Stream elem -> Stream elem
> sum s = 0 ≺ sum s + s
