
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

{-# LANGUAGE UnicodeSyntax #-}
module QuickTest (Probes, Property, (-->), (==>))
where
import Data.List

type Probes   a = [a]
type Property a = a -> Bool

infixr 1 -->, ==>
(-->) :: Probes a -> Property b -> Property (a -> b)
(==>) :: Probes a -> (a -> Property b) -> Property (a -> b)

probes --> prop = \f -> and [prop   (f x) | x <- probes]
probes ==> prop = \f -> and [prop x (f x) | x <- probes]

-- 3.5.1
ordered :: (Ord a) => Property [a]
ordered []       = True
ordered [x]      = True
ordered (y:x:xs) = y <= x && ordered (x:xs)

-- 3.5.2
-- The number of permutations of n distinct objects is n factorial (n!).
permutation :: (Eq a) => [a] -> Probes [a]
permutation [] = [[]]
permutation xs = do x <- xs
                    let y = delete x xs
                    ys <- permutation y
                    return $ x : ys

-- 3.5.3
runsTest :: (Ord a) => [[a]] -> Bool
runsTest []     = True
runsTest (x:xs) = ordered x && runsTest xs

isqrt :: Integer -> Integer
isqrt n = loop 0 3 1
  where loop i k s | s <= n    = loop (i + 1) (k + 2) (s + k)
                   | otherwise = i

-- 3.5.4
-- isIntegerSqrtNTimes returns true if the outcome is correct
-- for all values of 1 to n.
-- Otherwise it returns false.
isIntegerSqrt :: Integer -> Bool
isIntegerSqrt x = isqrt x == floor (sqrt (fromIntegral x))

isIntegerSqrtNTimes :: Integer -> Bool
isIntegerSqrtNTimes n
    | n > 1     = isIntegerSqrt n && isIntegerSqrtNTimes (n-1)
    | otherwise = True

-- 3.5.5
infixr 4 ⊗
(⊗) :: Probes a -> Probes b -> [[(a, b)]]
a ⊗ b = map (zip a) (permutations b)

niftySort :: [a] -> [a]
niftySort _ = []

trustedSort :: (Ord a) => [a] -> [a]
trustedSort = sort

