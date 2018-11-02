module WordNumber where
  import Data.List (intersperse)

  digitToWord :: Int -> String
  digitToWord n = case n of
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
  digits 0 = []
  digits n = digits (n `div` 10) ++ [n `mod` 10] 

  wordNumber :: Int -> String
  wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n