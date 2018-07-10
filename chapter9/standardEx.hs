myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny bool list =
  myOr $ map bool list

myRecElem :: Eq a => a -> [a] -> Bool
myRecElem _ []= False
myRecElem x (y:ys) = x == y || myRecElem x ys

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x list = myAny (x ==) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f list = squish (map f list)

squishAgain :: [[a]] -> [a]
squishAgain list = squishMap (id) list

myMaximumBy :: (a -> a -> Ordering)
               -> [a] -> a
myMaximumBy f (x1:x2:xs) =
  case (length xs == 0) of
    True -> case (f x1 x2) of
      LT -> x2
      EQ -> x2
      GT -> x1
    False -> case (f x1 x2) of
      LT -> myMaximumBy f $ x2 : xs
      EQ -> myMaximumBy f $ x2 : xs
      GT -> myMaximumBy f $ x1 : xs

myMinimumBy :: (a -> a -> Ordering)
               -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:xs) = go f x xs
  where go f x (y:[]) =
          case (f x y) of
            LT -> x
            EQ -> y
            GT -> y
        go f x (y:ys) = go f (go f x [y]) ys

myMaximum :: (Ord a) => [a] -> a
myMaximum list = myMaximumBy compare list

myMinimum :: (Ord a) => [a] -> a
myMinimum list = myMinimumBy compare list
