module Exercise2
where
import Data.Char

-- 2.2.1
equal :: String -> String -> Bool
a `equal` b = map toLower a == map toLower b


-- 2.2.2
isNumeral :: String -> Bool
isNumeral a = all isDigit a

isBlank :: String -> Bool
isBlank a = length a == 0


-- 2.2.3
fromDigit :: Char -> Int
fromDigit a = digitToInt a

toDigit :: Int -> Char
toDigit a = intToDigit a


-- 2.2.4
shiftOne :: Char -> Char
shiftOne 'z' = 'a'
shiftOne 'Z' = 'A'
shiftOne ' ' = ' '
shiftOne  c  = succ c

shift :: Int -> Char -> Char
a `shift` b
    | a >= 1  = (a-1) `shift` (shiftOne b)
    | a == 0 = b


-- 2.2.5
-- *Char> 19 `caesar` msg
-- "FABER EST SUAE QUISQUE FORTUNAE APPIUS CLAUDIUS CAECUS DICTUM ARCNUM EST NEUTRON"
caesar :: Int -> String -> String
a `caesar` b
    | a >= 1 = (a-1) `caesar` (map shiftOne b)
    | a == 0 = b

msg :: String
msg = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

