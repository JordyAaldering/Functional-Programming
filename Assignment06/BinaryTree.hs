
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

{-# LANGUAGE UnicodeSyntax #-}
module BinaryTree where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

-- 6.1.1
instance (Eq elem) => Eq (Tree elem) where
  Empty == Empty = True
  Empty == _node = False
  _node == Empty = False
  (Node l1 a r1) == (Node l2 b r2) = l1 == l2 && a == b && r1 == r2

instance (Ord elem) => Ord (Tree elem) where
  compare Empty Empty = EQ
  compare Empty _node = LT
  compare _node Empty = GT
  compare (Node _ a _) (Node _ b _) = compare a b

-- Declaring the instance declarations yourself can be
-- useful in some cases, but usually the compiler can
-- work out the correct instance declarations itself.

-- Comparing two trees can be useful. For instance
-- when checking if the output of an algorithm is
-- correct by comparing it to an algorithm you know
-- works correctly.

