
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

-- 2.1.1
-- We added underscores/extra symbols to the names,
-- because occurrences would be ambiguous otherwise.
not_ :: Bool -> Bool
not_ True  = False
not_ False = True

show_ :: Bool -> Bool
show_ a = a


-- 2.1.2
and_ :: (Bool, Bool) -> Bool
and_ (False, _) = False
and_ (True,  a) = a

or_ :: (Bool, Bool) -> Bool
or_ (False, a) = a
or_ (a, False) = a
or_ (_, _)     = True

xor_ :: (Bool, Bool) -> Bool
xor_ (False, a) = a
xor_ (a, False) = a
xor_ (_, _)     = False

equals_ :: (Bool, Bool) -> Bool
equals_ (False, a) = not a
equals_ (True,  a) = a


-- 2.1.3
(&&&) :: Bool -> Bool -> Bool
False &&& _ = False
True  &&& a = a

(|||) :: Bool -> Bool -> Bool
False ||| a = a
a ||| False = a
_ ||| _     = True

(^||) :: Bool -> Bool -> Bool
False ^|| a = a
a ^|| False = a
_ ^|| _     = False

(===) :: Bool -> Bool -> Bool
False === a = not a
True  === a = a

