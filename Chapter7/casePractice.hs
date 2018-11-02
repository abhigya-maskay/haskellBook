functionC :: (Ord a) => a -> a -> a
functionC x y = case (x > y) of
    True -> x
    False -> y

ifEvenAdd2 :: (Integral a) => a -> a
ifEvenAdd2 n = case even n of
    True -> n+2
    False -> n

nums :: (Num a, Ord a) => a -> a
nums x = case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0