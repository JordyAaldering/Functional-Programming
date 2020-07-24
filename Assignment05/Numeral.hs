
-- Exercise 5.2
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

module Numeral
where

type Base  = Integer
type Digit = Integer

-- Exercise 5.2.1
msdf, lsdf :: Base -> [Digit] -> Integer
msdf base = foldl (\c s -> s + base * c) 0
lsdf base = foldr (\c s -> s * base + c) 0

-- Exercise 5.2.2
-- By filling in the reverse of the list,
-- one can be defined in terms of the other.
msdfr, lsdfr :: Base -> [Digit] -> Integer
msdfr b xs = lsdf b (reverse xs)
lsdfr b xs = msdf b (reverse xs)
