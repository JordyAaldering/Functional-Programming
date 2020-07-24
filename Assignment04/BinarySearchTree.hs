
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

{-# LANGUAGE UnicodeSyntax #-}
module BinarySearchTree
where
import BinaryTree hiding (member)
import QuickTest

registry :: Tree String
registry = Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty

-- 4.4.1
-- The difference is that
-- here '(Ord elem)' is used,
-- and in exercise 4.1.6 '(Eq elem)' is used.
member :: (Ord elem) => elem -> Tree elem -> Bool
member _ (Empty)      = False
member e (Node l a r) = member e l || a == e || member e r

-- 4.4.2
insert :: (Ord elem) => elem -> Tree elem -> Tree elem
insert e Empty        = Node Empty e Empty
insert e (Node l a r) = Node (insert e l) a r

-- 4.4.3
delete :: (Ord elem) => elem -> Tree elem -> Tree elem
delete e Empty = Empty
delete e (Node l a r)
    | e == a     = Empty
    | member e l = Node (delete e l) a r
    | member e r = Node l a (delete e r)

-- 4.4.4
-- isSearchTree :: (Ord elem) => Tree elem -> Bool
-- isSearchTree Empty        = True
-- issearchTree (Node l a r) = 

-- trees :: [elem] -> Probes (Tree elem)
-- trees []     = 
-- trees (e:es) = 
