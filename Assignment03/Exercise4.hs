module Exercise4
where
import Prelude hiding (filter)
import Data.List

-- The assignment would not compile!
-- We tried multiple things, like changing "List" to "Data.List",
-- but those things would not work.
-- So we did the best we could without being able to test our code.

data Base = A | C | G | T
  deriving (Eq, Ord)

instance Show Base where
  showsPrec _ A = showChar 'A'
  showsPrec _ C = showChar 'C'
  showsPrec _ G = showChar 'G'
  showsPrec _ T = showChar 'T'
  showList      = foldr (.) id . map shows

base :: Char -> Maybe Base
base 'A' = Just A
base 'C' = Just C
base 'G' = Just G
base 'T' = Just T
base  _  = Nothing

type DNA     = [Base]
type Segment = [Base]

dna :: DNA
dna = [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

mm :: DNA
mm = filter base
   "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
   \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
   \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
   \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
   \GACAATTTAATAT\
   \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
   \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
   \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
   \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

readDNA :: FilePath -> IO [Base]
readDNA path
  = do x <- readFile path
       return (filter base x)

-- 3.4.1
contains :: Segment -> DNA -> Bool
x `contains` xs = xs `isInfixOf` x

-- 3.4.2
longestOnlyAs :: DNA -> Int
longestOnlyAs x = length $ last $ sort $ runs x

runs :: DNA -> [DNA]
runs "" = []
runs xs = munched : runs (drop (length munched) xs)
  where munched = munch xs


-- 3.4.3
-- longestAtMostTenAs :: DNA -> Integer
-- longestAtMostTenAs x = ?

