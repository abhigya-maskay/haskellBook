myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x = foldr ((||) . (== x)) False

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then (a : b) else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = (squishMap (id))

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            ->  a
myMaximumBy f xs = foldr (\a b -> if f a b == GT then a else b) (head xs) (tail xs)

myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy f xs = foldr (\a b -> if f a b == LT then a else b) (head xs) (tail xs)
