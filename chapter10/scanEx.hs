fibs = takeWhile (\x -> x < 100)  (1 : scanl (+) 1 fibs)
fibsN x = fibs !! x

factorial = take 20 (scanl (*) 1 [2..])
