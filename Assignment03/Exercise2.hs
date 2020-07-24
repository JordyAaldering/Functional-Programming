module Exercise2
where
import Data.List
import Data.Bool

-- 3.2.1
allTrue :: [Bool] -> Bool
allTrue []     = True
allTrue (x:xs) = x && allTrue xs

-- 3.2.2
allFalse :: [Bool] -> Bool
allFalse [] = True
allFalse (x:xs) = not x && allFalse xs

-- 3.2.3
member :: (Eq a) => a -> [a] -> Bool
member x l = x `elem` l

-- 3.2.4
smallest :: [Int] -> Int
smallest [x] = x
smallest (x1:x2:xs)
    | x1 < x2   = smallest (x1:xs)
    | otherwise = smallest (x2:xs)

-- 3.2.5
largest :: [Int] -> Int
largest [x] = x
largest (x1:x2:xs)
    | x1 > x2   = largest (x1:xs)
    | otherwise = largest (x2:xs)

