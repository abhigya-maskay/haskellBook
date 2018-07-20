module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "Zero"
    1 -> "One"
    2 -> "Two"
    3 -> "Three"
    4 -> "Four"
    5 -> "Five"
    6 -> "Six"
    7 -> "Seven"
    8 -> "Eight"
    9 -> "Nine"
    _ -> "Unknown"

digits :: Int -> [Int]
digits n = go n 10 []
  where go n counter collector=
          case (div n counter == 0) of
            True -> concat[[mod n counter], collector]
            False -> go (div n counter) 10  $ concat [[mod n counter], collector]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n
