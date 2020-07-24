> {-# LANGUAGE UnicodeSyntax #-}
> module Hardware where
> import Unicode

> data Bit  =  O | I
>   deriving (Eq, Ord, Show)

> infixr 3 ∧
> (∧) ∷ Bit → Bit → Bit
> O ∧ _b  =  O
> I ∧ b   =  b

> infixr 2 ∨
> (∨) ∷ Bit → Bit → Bit
> O ∨ b   =  b
> I ∨ _b  =  I

> infixr 4 ⊕
> (⊕) ∷ Bit → Bit → Bit
> O ⊕ O  =  O
> O ⊕ I  =  I
> I ⊕ O  =  I
> I ⊕ I  =  O

> type Carry  =  Bit

