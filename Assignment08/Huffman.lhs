
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
>
> module Huffman
> where
> import Unicode
> import Satellite
> import Tree
> import Data.List

-------------------------------------------------------------------------------

Warm-up: constructing a frequency table.

> frequencies  ∷  (Ord char) ⇒ [char] → [With Int char]
> frequencies x = map(\a -> (length a :- head a))(group (sort x))

-------------------------------------------------------------------------------

Constructing a Huffman tree.

> huffman ∷ [With Int char] → Tree char
> huffman xs = satellite . branch . sort $ map (\(a :- b)->(a :- Leaf b)) xs
>   where branch [x] = x
>         branch ((a :- b):(c :- d):xs) = branch (sort ((a+c :- (b:^:d)) : xs))

-------------------------------------------------------------------------------

Encoding ASCII text.
1.

> data Bit = O | I
>   deriving (Show, Eq, Ord)

> encode ∷ (Eq char) ⇒ Tree char → [char] → [Bit]
> encode t [] = []
> encode t (x:xs) = replaceWithBytes (codes t) x ++ encode t (xs)

> replaceWithBytes :: (Eq char) ⇒ [(char, [Bit])] → char → [Bit]
> replaceWithBytes ((a, b):ys) x
>   | a == x = b
>   | otherwise = replaceWithBytes ys x

> codes ∷ Tree char → [(char, [Bit])]
> codes t = fixCode t []

> fixCode :: Tree char -> [Bit] -> [(char, [Bit])]
> fixCode (Leaf t) b = [(t, b)]
> fixCode (t1:^:t2) b = fixCode t1 (b++[O]) ++ fixCode t2 (b++[I])

2.

> englishFreq :: [With Int Char]
> englishFreq = [(8167:-'a'),(1492:-'b'),(2782:-'c'),(4253:-'d'),(12702:-'e'),(2228:-'f'),(2015:-'g'),(6094:-'h'),(6966:-'i'),(153:-'j'),(772:-'k'),(4025:-'l'),(2406:-'m'),(6749:-'n'),(7507:-'o'),(1929:-'p'),(95:-'q'),(5987:-'r'),(6327:-'s'),(9056:-'t'),(2758:-'u'),(978:-'v'),(2360:-'w'),(150:-'x'),(1974:-'y'),(74:-'z'),(21375:-' '),(8623:-'.'),(8616:-',')]

-------------------------------------------------------------------------------

Decoding a Huffman binary.

> decode ∷ (Eq char) => Tree char → [Bit] → [char]
> decode t b = decoder (codes t) [] b

> decoder :: (Eq char) => [(char, [Bit])] -> [Bit] -> [Bit] -> [char]
> decoder c b [] = []
> decoder c b (x:xs) = replaceWithChar c c (b++[x]) xs

> replaceWithChar :: (Eq char) => [(char, [Bit])] -> [(char, [Bit])] -> [Bit] -> [Bit] -> [char]
> replaceWithChar c ((a, b):xs) b1 b2
>   | b == b1 = [a] ++ decoder c [] b2
>   | xs == [] = decoder c b1 b2
>   | otherwise = replaceWithChar c xs b1 b2

-------------------------------------------------------------------------------

Some test data.

> hw, why ∷ String
> hw =
>   "hello world"

code = huffman (frequencies hw)
encode code hw
decode code it
decode code it == hw

> why =
>   "As software becomes more and more complex, it\n\
>   \is  more  and  more important to structure it\n\
>   \well.  Well-structured  software  is  easy to\n\
>   \write,   easy   to   debug,  and  provides  a\n\
>   \collection  of modules that can be re-used to\n\
>   \reduce future programming costs. Conventional\n\
>   \languages place a conceptual limit on the way\n\
>   \problems   can   be  modularised.  Functional\n\
>   \languages  push  those  limits  back. In this\n\
>   \paper we show that two features of functional\n\
>   \languages    in    particular,   higher-order\n\
>   \functions and lazy evaluation, can contribute\n\
>   \greatly  to  modularity.  Since modularity is\n\
>   \the key to successful programming, functional\n\
>   \languages  are  vitally important to the real\n\
>   \world."

code = huffman (frequencies why)
encode code why
decode code it
decode code it == why
