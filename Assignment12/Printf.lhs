
Exercise 12.4
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Printf
> where


> data D = D deriving (Show)
> data F = F deriving (Show)
> data S = S deriving (Show)

> infixr 4 &
> (&) :: a -> b -> (a, b)
> a & b = (a, b)

> type family Arg d r :: *

> printf :: (Format d) => d -> Arg d String
> printf d = format d id ""

> class Format d where
>   format :: d -> (String -> a) -> String -> Arg d a


> type instance Arg D r        = Int    -> r
> type instance Arg S r        = String -> r
> type instance Arg F r        = Float  -> r
> type instance Arg String r   = r
> type instance Arg (d1, d2) r = Arg d1 (Arg d2 r)

> instance Format D where
>   format D c o = \i -> c (o ++ show i)
>
> instance Format S where
>   format S c o = \s -> c (o ++ s)
>
> instance Format F where
>   format F c o = \f -> c (o ++ show f)
>
> instance Format String where
>   format s c o = c (o ++ s)
>
> instance (Format l, Format r) => Format (l, r) where
>   format (l, r) = format l . format r


*Printf> printf D 51
"51"
*Printf> printf ("I am " & D & " years old.") 51
"I am 51 years old."
*Printf> printf ("I am " & D & " " & S & " old.") 1 "year"
"I am 1 year old."
*Printf> fmt = "Color " & S & ", Number " & D & ", Float " & F
*Printf> printf fmt "purple" 4711 3.1415
"Color purple, Number 4711, Float 3.1415"

