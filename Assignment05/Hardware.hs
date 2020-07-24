
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

module Hardware
where

data Bit = O | I
  deriving (Eq, Ord, Show)

infixr 3 ∧
(∧) :: Bit -> Bit -> Bit
O ∧ _ = O
I ∧ b = b

infixr 2 ∨
(∨) :: Bit -> Bit -> Bit
O ∨ b = b
I ∨ _ = I

infixr 4 ⊕
(⊕) :: Bit -> Bit -> Bit
O ⊕ O = O
O ⊕ I = I
I ⊕ O = I
I ⊕ I = O

-- 5.6.1
mapr :: ((a, state) -> (b, state)) -> (([a], state) -> ([b], state))
mapr f = \(xs, e) -> foldr g ([], e) xs
  where g x (ys, e) = let (y, es) = f (x, e) in (y:ys, es)

type Carry = Bit

-- 5.6.2
halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (x, y) = (x ⊕ y, x ∧ y)

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((x, y), z) = (u ⊕ z, v ∨ ((x ∨ y) ∧ z))
  where (u, v) = halfAdder (x, z)

adder :: ([(Bit, Bit)], Carry) -> ([Bit], Carry)
adder (xs,y) = mapr fullAdder (xs, y)
