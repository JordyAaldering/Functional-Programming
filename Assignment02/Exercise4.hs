
-- 2.4.1
swapInt :: (Int, Int) -> (Int, Int)
swapInt (a, b) = (b, a)

swapChar :: ([Char], Char) -> (Char, [Char])
swapChar (a, b) = (b, a)

swapString :: (String, Char) -> (Char, String)
swapString (a, b) = (b, a)


-- 2.4.2
-- The other three functions could still be used, but only when the names are changed,
-- because Haskell does not allow function overloading.
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)


-- 2.4.3
-- (Int, (Char, Bool)) takes two components, namely; Int and (Char, Bool).
-- (Int, Char, Bool) takes three components, namely; Int, Char and Bool.
formatSplit :: (Int, (Char, Bool)) -> (Int, Char, Bool)
formatSplit (a, (b, c)) = (a, b, c)

formatCombine :: (Int, Char, Bool) -> (Int, (Char, Bool))
formatCombine (a, b, c) = (a, (b, c))

