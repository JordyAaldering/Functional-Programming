
-- Exercise 5.1
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

module Exercise1
where
import Data.List
import Data.Bool

-- Exercise 5.1.1
allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True

-- Exercise 5.1.2
allFalse :: [Bool] -> Bool
allFalse l = not (foldr (||) False l)

-- Exercise 5.1.3
member :: (Eq a) => a -> [a] -> Bool
member a l = foldr (||) False (map (a ==) l)

-- Exercise 5.1.4
smallest ::  [Int] -> Int
smallest = foldr chooseLowest maxBound
    where
    chooseLowest x1 x2
        | x1 < x2 = x1
        | otherwise = x2

-- Exercise 5.1.5
largest :: [Int] -> Int
largest = foldr chooseHighest minBound
    where
    chooseHighest x1 x2
        | x1 > x2 = x1
        | otherwise = x2

