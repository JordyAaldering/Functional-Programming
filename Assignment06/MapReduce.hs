
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

{-# LANGUAGE UnicodeSyntax #-}
module MapReduce where
import Prelude hiding (Monoid)
import Hardware


class Monoid a where
  ε   :: a
  (•) :: a -> a -> a

reduce :: (Monoid m) => [m] -> m
reduce = foldr (•) ε


-- 6.2.1
-- True
newtype SetTrue = SetTrue {getTrue :: Bool}
  deriving (Show)
instance Monoid SetTrue where
  ε     = SetTrue True
  x • y = SetTrue True

-- False
newtype SetFalse = SetFalse {getFalse :: Bool}
  deriving (Show)
instance Monoid SetFalse where
  ε     = SetFalse False
  x • y = SetFalse False

-- And
newtype And = And {getAnd :: Bool}
  deriving (Eq, Show)
instance Monoid And where
  ε     = And True
  x • y = And (getAnd x && getAnd y)

-- Or
newtype Or = Or {getOr :: Bool}
  deriving (Eq, Show)
instance Monoid Or where
  ε     = Or True
  x • y = Or (getOr x || getOr y)

-- Xor
newtype Xor = Xor {getXor :: Bool}
  deriving (Eq, Show)
instance Monoid Xor where
  ε     = Xor True
  x • y = Xor ((getXor x || getXor y) && not (getXor x && getXor y))

-- All
newtype All = All {getAll :: Bool}
  deriving (Eq, Show)
instance Monoid All where
  ε     = All True
  x • y = All (getAll x == getAll y)

-- Left'
newtype Left' = Left' {getLeft :: Bool}
  deriving (Eq, Show)
instance Monoid Left' where
  ε     = Left' True
  x • y = Left' (getLeft x)

-- Right'
newtype Right' = Right' {getRight :: Bool}
  deriving (Eq, Show)
instance Monoid Right' where
  ε     = Right' True
  x • y = Right' (getRight x)


-- 6.2.2
-- ...


-- 6.3
-- newtype OrdList elem = Ord [elem]
--   deriving (Show)

-- instance (Ord elem) => Monoid (OrdList elem) where
--   ε     = Ord []
--   x • y = Ord (OrdList x ++ OrdList y)


-- 6.4.1
-- ...


-- 6.4.2
-- foldm :: (a -> a -> a) -> a -> ([a] -> a)


-- 6.4.3
-- foldm :: (a -> a -> a) -> a -> ([a] -> a)


-- 6.5.1
kpg :: (Bit, Bit) -> (Carry -> Carry)
kpg (O, O) = \_ -> O  -- kill
kpg (O, I) = \c -> c  -- propagate
kpg (I, O) = \c -> c  -- propagate
kpg (I, I) = \_ -> I  -- generate

data KPG = K | P | G

