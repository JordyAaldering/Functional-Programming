
-- 2.6.1
-- There is only one function of type 'Int' to 'Int', namely; Int -> Int.
-- There is an infinite amount of functions of type 'a' to 'a',
-- because you can introduce your own types and use them there.
-- But the only total function of type 'a' -> 'a' is the identity function.
-- a)
addOne :: Int -> Int
addOne a = a + 1

-- b)
identity :: a -> a
identity a = a

-- c)
mul :: (Int, Int) -> Int
mul (a, b) = a * b

-- d)
shrink :: (a, a) -> a
shrink (a, _) = a

-- e)
first :: (a, b) -> a
first (a, _) = a


-- 2.6.2
-- From here on we had a lot of trouble with the exercise so we did the best we could.
-- a)
replaceSecond :: (a, a) -> (a, a)
replaceSecond (a, _) = (a, a)

-- b)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

-- c)
doFunction :: (a -> b) -> a -> b
doFunction a = a

-- d)
first_ :: (a, x) -> a
first_ (a, _) = a

-- e)
fillFunction :: (x -> a -> b, a, x) -> b
fillFunction (b, a, x) = (b x) a

-- f)
fillTwoFunctions :: (a -> b, x -> a, x) -> b
fillTwoFunctions (b, a, x) = b (a x)

-- g)
combinedFunctions :: (x -> a -> b, x -> a, x) -> b
combinedFunctions (b, a, x) = (b x) (a x)


-- 2.6.3
-- Again, there is an infinite amount of functions of type 'a' to 'a',
-- because you can introduce your own types and use them there.
-- But the only total function of type 'a' -> 'a' is the identity function.
-- a)
recursiveInt :: Int -> (Int -> Int)
recursiveInt a = recursiveInt a

-- b)
zeroFunction :: (Int -> Int) -> Int
zeroFunction a = a 0

-- c)
recursion :: a -> (a -> a)
recursion a = recursion a

-- d)
moreRecursion :: (a -> a) -> a
moreRecursion a = moreRecursion a  

