eftBool :: Bool -> Bool -> [Bool]
eftBool True False = []
eftBool False True = [False, True]
eftBool True True = [True]
eftBool False False = [False]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd a b = if a > b then [] else
    [a] ++ takeWhile (<= b) (eftOrd (succ a) b)

eftInt :: Int -> Int -> [Int]
eftInt a b = if a > b then [] else
    [a] ++ takeWhile (<= b) (eftInt (a + 1) b)


eftChar :: Char -> Char -> [Char]
eftChar a b = if a > b then [] else
    [a] ++ takeWhile (<=b) (eftChar (succ a) b)