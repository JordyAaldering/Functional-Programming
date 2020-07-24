
Exercise 10.1
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> {-# LANGUAGE UnicodeSyntax #-}
> module Main
> where
> import Text.Printf
> import System.Environment

> type Amount = (Int, Int, Int)

> toString :: Amount -> String
> toString (l,w,c) = printf "%d %d %d" l w c

> incrAmount :: Amount -> Amount -> Amount
> incrAmount (l1,w1,c1) (l2,w2,c2) = (l1+l2, w1+w2, c1+c2)

> readAmount :: String -> IO Amount
> readAmount path = do
> 	file <- readFile path
> 	let amountL = length (lines file)
> 	let amountW = length (words file)
> 	let amountC = length file
> 	let amount  = (amountL, amountW, amountC)
> 	putStrLn (toString amount ++ " " ++ path)
> 	return amount

> main :: IO ()
> main = do
> 	args <- getArgs
> 	amounts <- mapM readAmount args
> 	let total = foldr incrAmount (0,0,0) amounts
> 	putStrLn(toString total ++ " total")

