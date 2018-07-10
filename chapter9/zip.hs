myZip :: [a] -> [b] -> [(a,b)]
myZip l1 l2=
  case (length l1 == 0 || length l2 == 0) of
    True -> []
    False -> [(head l1, head l2)] ++ (myZip (tail l1) (tail l2))

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f l1 l2 =
  case (length l1 == 0 || length l2 == 0) of
    True -> []
    False -> [f (head l1) (head l2)] ++ myZipWith f (tail l1) (tail l2)
