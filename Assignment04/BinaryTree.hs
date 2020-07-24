
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

{-# LANGUAGE UnicodeSyntax #-}
module BinaryTree
where
import Data.Bool

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

-- 4.1.1
figureOne :: Tree Char
figureOne = Node (Node Empty 'a' (Node Empty 'b' Empty)) 'c' (Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty))

-- 4.1.2
ex1 :: Tree Integer
ex1 = Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
--   4711
--  /    \
-- ϵ      815
--     /   \
--    ϵ      42
--        /  \
--       ϵ      ϵ

ex2 :: Tree String
ex2 = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
--               Ralf
--              /    \
--         Peter      ϵ
--        /     \
--   Frits       ϵ
--  /     \
-- ϵ          ϵ

ex3 :: Tree Char
ex3 = Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)
--     k
--    / \
--   a   z
--  / \ / \
-- ϵ   ϵϵ   ϵ

-- 4.1.3
size :: Tree elem -> Int
size Empty        = 0
size (Node l _ r) = size l + 1 + size r 

-- 4.1.4
minHeight, maxHeight :: Tree elem -> Int
minHeight Empty        = 0
minHeight (Node l _ r) = min (minHeight l) (minHeight r) + 1
maxHeight Empty        = 0
maxHeight (Node l _ r) = max (maxHeight l) (maxHeight r) + 1 

-- 4.1.5
-- maxHeight <= size - minHeight + 1
-- The maximal height can't be higher than: the size minus the minimal height plus one.
-- You have to add one, because the root node has to be used by both the minimal and maximal path.

-- 4.1.6
member :: (Eq elem) => elem -> Tree elem -> Bool
member _ Empty        = False
member e (Node l a r) = member e l || a == e || member e r

-- 4.2.1
preorder, inorder, postorder :: Tree elem -> [elem]
preorder  Empty        = []
preorder  (Node l a r) = [a] ++ preorder l ++ preorder r
inorder   Empty        = []
inorder   (Node l a r) = inorder l ++ [a] ++ inorder r
postorder Empty        = []
postorder (Node l a r) = postorder l ++ postorder r ++ [a]

-- 4.2.2
layout :: (Show elem) => Tree elem -> String
layout Empty = ""
layout t     = layoutP t '-' 1 ++ ['\n']

layoutP :: (Show elem) => Tree elem -> Char -> Int -> String
layoutP Empty _ _        = ""
layoutP (Node l a r) c i
    | empty l && empty r =                                         [c] ++ show a
    | empty l            =                                         [c] ++ show a ++ "\n" ++ pad i ++ layoutP r '\\' (i+1)
    | empty r            = pad i ++ layoutP l '/' (i+1) ++ "\n" ++ [c] ++ show a
    | otherwise          = pad i ++ layoutP l '/' (i+1) ++ "\n" ++ [c] ++ show a ++ "\n" ++ pad i ++ layoutP r '\\' (i+1) ++ "\n"

pad :: Int -> String
pad i = concat["   " | i <- [0..i]]

empty :: Tree elem -> Bool
empty Empty = True
empty _     = False

-- 4.3.1
build :: [elem] -> Tree elem
build []     = Empty
build (e:es) = Node Empty e (build es)

-- 4.3.2
balanced :: [elem] -> Tree elem
balanced []     = Empty
balanced (e:es) = Node (balanced (leftHalve es)) e (balanced (rightHalve es)) 

leftHalve :: [elem] -> [elem]
leftHalve [] = []
leftHalve l  = take ((length l) `div` 2) l

rightHalve :: [elem] -> [elem]
rightHalve [] = []
rightHalve l  = drop ((length l) `div` 2) l

-- 4.3.3
-- Harry Hacker is a quacksalver,
-- because for a tree of size n, n nodes have to be made.
-- This would take O(n) time, not O(log(n)).

