myOr :: [Bool] -> Bool
myOr = elem True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f list = myOr (map f list)

myElemRecursive :: Eq a => a -> [a] -> Bool
myElemRecursive _ [] = False
myElemRecursive element list = (== element) (head list) || myElemRecursive element (tail list)

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny element list = myAny (== element) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = myReverse (tail list) ++ [head list]

squish :: [[a]] -> [a]
squish [] = []
squish list = head list ++ squish (tail list)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f list = concat $ map f list

squishAgain :: [[a]] -> [a]
squishAgain list = squishMap id list

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f list = foldr max (head list) (tail list) where
  max = \a b -> case f a b of
    LT -> b
    _ -> a

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f list =  foldr min (head list) (tail list) where
  min = \a b -> case f a b of
    LT -> a
    _ -> b

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare 

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare