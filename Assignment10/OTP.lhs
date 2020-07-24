
Exercise 10.3
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

> {-# LANGUAGE UnicodeSyntax #-}
> module Main
> where
> import Data.Char
> import Data.List.Split
> import System.Random
> import System.Environment

> readInput :: IO [String]
> readInput = do
> 	line <- getLine
> 	let guess = splitOn " " line
> 	return guess

> caesar :: (Int -> Int -> Int) -> Char -> Int -> Char
> caesar operation c n
> 	| i < 32  = c
> 	| i >= 32 = chr (((i - 32 `operation` n) `mod` 96) + 32)
> 	where i   = ord c

> encrypt, decrypt :: Char -> Int -> Char
> encrypt c n = caesar (+) c n
> decrypt c n = caesar (-) c n

> generateFile :: (Char -> Int -> Char) -> String -> String -> IO ()
> generateFile operation inputFile outputFile = do
> 	readInput <- readFile inputFile
> 	generated <- mapM (\c -> do r <- getStdRandom random; return $ operation c r) readInput
> 	writeFile outputFile generated

> main :: IO ()
> main = do
> 	setStdGen (mkStdGen 2144)
> 	putStrLn "Enter operation type, input file name and output file name:"	
> 	input <- readInput
> 	let operation  = input !! 0
> 	let inputFile  = input !! 1
> 	let outputFile = input !! 2
> 	case operation of
> 		"encrypt" -> generateFile encrypt inputFile outputFile
> 		"decrypt" -> generateFile decrypt inputFile outputFile

