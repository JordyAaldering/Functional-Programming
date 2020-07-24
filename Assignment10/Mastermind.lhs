
Exercise 10.2
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> {-# LANGUAGE UnicodeSyntax #-}
> module Mastermind
> where
> import System.Random
> import Control.Monad
> import Text.Printf

> type Code = [String]

> colors :: [String]
> colors = ["white", "silver", "green", "red", "orange", "pink", "yellow", "blue"]

> randomInt :: Int -> IO Int
> randomInt n = getStdRandom (randomR (0, n-1))

> randomColor :: IO String
> randomColor = do
> 	y <- randomInt (length colors)
> 	let x = colors !! y
> 	return x

> randomCode :: Int -> IO [String]
> randomCode codeLength = replicateM codeLength (randomColor)

> readInput :: IO [String]
> readInput = do
> 	line <- getLine
> 	let guess = words line
> 	return guess

> correctPos :: [String] -> [String] -> Int
> correctPos [] _ = 0
> correctPos _ [] = 0
> correctPos (x:xs) (y:ys)
> 	| x == y    = 1 + correctPos xs ys
> 	| otherwise = correctPos xs ys

> deleteElemFromList :: Int -> [a] -> [a]
> deleteElemFromList _ [] = []
> deleteElemFromList n (x:xs)
> 	| n == 0    = xs
> 	| otherwise = x : deleteElemFromList (n-1) xs

> correctCol :: [String] -> [String] -> [String] -> Int
> correctCol _ _ []      = 0
> correctCol m [] (y:ys) = correctCol m m ys
> correctCol m (x:xs) (y:ys)
> 	| x == y = 1 + correctCol (mNew) (mNew) ys
> 	| otherwise = correctCol m xs (y:ys)
> 	where mNew = deleteElemFromList ((length m)- (length (x:xs))) m

> game :: Int -> [String] -> IO ()
> game 0 _ = do putStrLn "You lost!"
> game triesLeft masterCode = do
> 	printf "Tries left: %d\n" triesLeft
> 	putStrLn "Enter a guess (in non-capital letters with a space between each color):"
> 	input <- readInput
> 	let corPos = correctPos masterCode input
> 	let corCol = correctCol masterCode masterCode input
> 	printf "Correct position: %d\nCorrect color but wrong position: %d\n\n" corPos (corCol-corPos)
> 	if corPos == length masterCode
> 		then putStrLn "For once you are not a disappointment to your family!"
> 		else game (triesLeft - 1) masterCode

> main :: IO()
> main = do
> 	let codeLength = 4
> 	let maxTries = 12
> 	masterCode <- randomCode codeLength
> 	game maxTries masterCode
> 	printf "The master code was: %s\n" (show masterCode)

