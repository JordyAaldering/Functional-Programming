> {-# LANGUAGE UnicodeSyntax #-}
> module OldMan
> where
> import Unicode

> type Verse = (Nr, Wrd)
> type Nr    = String
> type Wrd   = String

> verse1, verse2, verse3, verse4, verse5, verse6, verse7, verse8, verse9, verse10 :: Verse
> verse1  = ("one",   "on my thumb")
> verse2  = ("two",   "on my shoe")
> verse3  = ("three", "on my knee")
> verse4  = ("four",  "on my door")
> verse5  = ("five",  "on my hive")
> verse6  = ("six",   "on my sticks")
> verse7  = ("seven", "up in heaven")
> verse8  = ("eight", "on my gate")
> verse9  = ("nine",  "on my spine")
> verse10 = ("ten",   "once again")

> poem :: [Verse]
> poem =  [verse1, verse2, verse3, verse4, verse5, verse6, verse7, verse8, verse9, verse10]

> number :: Verse → String
> number (n, _w) = n

> word :: Verse → String
> word (_n, w) = w 

> thisOldMan :: String
> thisOldMan = showVerse(1) ++ showVerse(2) ++ showVerse(3) ++ showVerse(4) ++ showVerse(5) ++ showVerse(6) ++ showVerse(7) ++ showVerse(8) ++ showVerse(9) ++ showVerse(10)

> showVerse :: Int → String
> showVerse (n) = "This man he played "       ++ number (poem !! n)
>              ++ ",\nHe played knick-knack " ++ word   (poem !! n)
>              ++ ";\nWith a knick-knack paddywhack,\nGive the dog a bone,\nThis old man came rolling home.\n\n"

