mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

makeTuples :: Integral a => [a] -> [a] -> [(a,a)]
makeTuples a b = [(x,y) | x <- a, y <- b]

makeTuples2 :: Integral a => [a] -> [a] -> [(a,a)]
makeTuples2 a b = [(x,y) | x <- a, y <- b, x < 50, y < 50]

findNumTuples :: [a] -> Int
findNumTuples a = length a
