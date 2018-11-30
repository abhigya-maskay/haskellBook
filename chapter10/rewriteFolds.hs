myOr :: [Bool] -> Bool
myOr = foldr (\a b -> if a==True then True else b) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if f a == True then True else b) False

myElem :: Eq a => a -> [a] -> Bool
myElem element list = foldr (\a b -> if a == element then True else b) False list

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 element = myAny (== element) 

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) [] 

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if (f a) then a:b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> a
myMaximumBy f list = foldr max (head list) (tail list) where
    max a b = case f a b of
        GT -> a
        _ -> b

myMinimumBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> a
myMinimumBy f list = foldr min (head list) (tail list) where
    min a b = case f a b of
        LT -> a
        _ -> b