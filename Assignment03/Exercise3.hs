module Exercise3
where
import Data.List
import Data.Bool

-- This is a useful pre-processing step prior to sorting a list,
-- because partitions of the list that are already sorted
-- won't have to be checked again, which will eventually save time.

runs :: (Ord a) => [a] -> [[a]]
runs [] = []
runs (x:xs) = y : runs ys
    where (y,ys) = split (x:xs)

split :: (Ord a) => [a] -> ([a], [a])
split [x] = ([x], [])
split (x:y:xs)
    | x > y       = ([x], (y:xs))
    | otherwise   = (x:ys, zs)
    where (ys,zs) = split (y:xs)

