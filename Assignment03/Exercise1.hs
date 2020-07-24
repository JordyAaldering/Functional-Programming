
-- Student names and numbers:
-- Thomas van Harskamp, s1007576
-- Jordy Aaldering, s1004292

module Exercise1
where
import Prelude hiding (Word)
import Data.List

type Word = String

wordList :: String -> [(Word, Int)]
wordList = sortOn snd . map count . group . sort . words . sanitize
  where
    count s  = (head s, length s)
    sanitize = filter (\x -> x /= '.' && x /= ',')

lorem :: String
lorem = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
        \nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam \
        \erat, sed diam voluptua. At vero eos et accusam et justo duo dolores \
        \et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
        \Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur \
        \sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et \
        \dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam \
        \et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea \
        \takimata sanctus est Lorem ipsum dolor sit amet."

