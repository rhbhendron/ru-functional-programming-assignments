module DNA where

import Data.List

-- Nucleobases or DNA-bases are the basic building blocks of
-- deoxyribonucleic acid (DNA).
-- Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

data Base  =  A | C | G | T
  deriving (Eq, Ord, Show)

type DNA      =  [Base]
type Segment  =  [Base]

exampleDNA :: DNA
exampleDNA = [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

contains :: Segment -> DNA -> Bool

containsI :: Segment -> DNA -> [Int]

longestOnlyAs :: DNA -> Int

longestAtMostTenAs :: DNA -> Int
-- Here be dragons!

toDNA :: String -> DNA
toDNA s = [ base | c <- s, (c',base) <- zip "ACGT" [A,C,G,T], c == c' ]

largerDNA :: DNA
largerDNA = toDNA
  "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
  \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
  \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
  \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
  \GACAATTTAATAT\
  \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
  \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
  \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
  \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

{- you can use the `fromFile` operator to run your function on an external file,
 - for example:
 -
 -  >>>  longestOnlyAs `fromFile` "mm1.dna"
 -  182
 -}

fileDNA :: (DNA -> a) -> FilePath -> IO a
fileDNA f fn = fmap (f.toDNA) (readFile fn)
infix 0 `fileDNA`
