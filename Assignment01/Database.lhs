> {-# LANGUAGE UnicodeSyntax #-}
> module Database
> where
> import Unicode

> type Person = (Name, Age, FavouriteCourse)
> type Name   =  String
> type Age    =  Integer
> type FavouriteCourse = String

> frits, peter, ralf :: Person
> frits  =  ("Frits",  33, "Algorithms and Data Structures")
> peter  =  ("Peter",  57, "Imperative Programming")
> ralf   =  ("Ralf",   33, "Functional Programming")
> thomas =  ("Thomas", 19, "Imperative Programming")
> jordy  =  ("Jordy",  19, "Object Orientation")

> students :: [Person]
> students =  [frits, peter, ralf, thomas, jordy]

> age :: Person → Age
> age (_n, a, _c) = a

> name :: Person → Name
> name (n, _a, _c) = n

> favouriteCourse :: Person → FavouriteCourse
> favouriteCourse (_n, _a, c) = c

> showPerson :: Person → String
> showPerson (n, a, c) = n ++ " " ++ show a ++ " " ++ c

> twins :: Person → Person → Bool
> twins (_, a1, _) (_, a2, _) = a1 == a2

> increaseAge :: Person → Person
> increaseAge (n, a, c) = (n, a + 1, c)

> incrementAgeByTwo :: [Person] → [Person]
> incrementAgeByTwo =  map (twice increaseAge)

> promoteAll :: [Person] → [Person]
> promoteAll =  map (\(n, a, c) -> ("dr " ++ n, a, c))

> findFrits :: [Person] → [Person]
> findFrits =  filter (\p → name p == "Frits")

> favouriteFP :: [Person] → [Person]
> favouriteFP =  filter (\p → favouriteCourse p == "Functional Programming")

> twenties :: [Person] → [Person]
> twenties =  filter (\p → age p >= 20 && age p < 30)

> favouriteFPAndTwenties :: [Person] → [Person]
> favouriteFPAndTwenties =  favouriteFP . twenties

> unique :: [Person] → [Person]
> unique (p : ps) = if p `elem` ps then unique ps else p : unique ps
> unique []       = []

> favouriteIP = filter (\p → favouriteCourse p == "Imperative Programming")
> favouriteIPOrTwenties students = unique (twenties students ++ favouriteIP students)

> twice :: (t → t) → t → t
> twice f x = f (f x)

