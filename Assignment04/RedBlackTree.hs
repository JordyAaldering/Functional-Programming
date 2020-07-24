
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

{-# LANGUAGE UnicodeSyntax #-}
module RedBlackTree
where
import QuickTest
import Data.Bool

data RedBlackTree elem
  =  Leaf
  |  Red   (RedBlackTree elem) elem (RedBlackTree elem)
  |  Black (RedBlackTree elem) elem (RedBlackTree elem)
  deriving (Show)

instance Functor RedBlackTree where
  fmap _ Leaf          = Leaf
  fmap f (Red   l a r) = Red   (fmap f l) (f a) (fmap f r)
  fmap f (Black l a r) = Black (fmap f l) (f a) (fmap f r)

-- 4.5.1
member :: (Ord elem) => elem -> RedBlackTree elem -> Bool
member _ Leaf   = False
member e (Red l a r)
    | e == a    = True
    | otherwise = member e l || member e r
member e (Black l a r)
    | e == a    = True
    | otherwise = member e l || member e r

-- 4.5.2
-- If a red node is at the top,
-- then not all conditions hold any more.
-- because every red node has to have a black parent,
-- which is not the case any more if a red node is at the top.
insert :: (Ord elem) => elem -> RedBlackTree elem -> RedBlackTree elem
insert e Leaf          = Red Leaf e Leaf
insert e (Red   l a r) = Red (insert e l) a r
insert e (Black l a r) = Black (insert e l) a r 

insert' :: (Ord elem) => elem -> RedBlackTree elem -> RedBlackTree elem
insert' e Leaf          = Red Leaf e Leaf
insert' e (Red   l a r) = Red (insert' e l) a r
insert' e (Black l a r) = flipNodes (black (insert' e l) a r)

flipNodes :: RedBlackTree elem -> RedBlackTree elem
flipNodes Leaf          = Leaf
flipNodes (Red   l a r) = Black (flipNodes l) a (flipNodes r)
flipNodes (Black l a r) = Red (flipNodes l) a (flipNodes r)

black :: RedBlackTree elem -> elem -> RedBlackTree elem -> RedBlackTree elem
black l a r = Red (toBlack (childL l)) (getElem l) (Black r a Leaf)

toBlack :: RedBlackTree elem -> RedBlackTree elem
toBlack Leaf          = Leaf
toBlack (Red   l a r) = Black l a r
toBlack (Black l a r) = Black l a r 

childL :: RedBlackTree elem -> RedBlackTree elem
childL (Red   l _ _) = l
childL (Black l _ _) = l

getElem :: RedBlackTree elem -> elem
getElem (Red   _ a _) = a
getElem (Black _ a _) = a

-- 4.5.3
isRedBlackTree :: RedBlackTree elem -> Bool
isRedBlackTree Leaf        = True
isRedBlackTree (Red _ _ _) = False
isRedBlackTree t           = conditionOne t && conditionTwo t

conditionOne :: RedBlackTree elem -> Bool
conditionOne Leaf          = True
conditionOne (Red l a r)
    | isRed l || isRed r   = False
    | otherwise            = conditionOne l && conditionOne r
conditionOne (Black l a r) = conditionOne l && conditionOne r

isRed :: RedBlackTree elem -> Bool
isRed (Red _ _ _) = True
isRed _           = False

conditionTwo :: RedBlackTree elem -> Bool
conditionTwo Leaf          = True
conditionTwo (Red   l _ r) = blackHeight l == blackHeight r && conditionTwo l && conditionTwo r
conditionTwo (Black l _ r) = blackHeight l == blackHeight r && conditionTwo l && conditionTwo r

blackHeight :: RedBlackTree elem -> Int
blackHeight Leaf          = 0
blackHeight (Red   l a r) = blackHeight l
blackHeight (Black l a r) = 1 + blackHeight l

-- redBlackTrees :: [elem] -> Probes (RedBlackTree elem)
-- redBlackTrees []     =
-- redBlackTrees (e:es) =

