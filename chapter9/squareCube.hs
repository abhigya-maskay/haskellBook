mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

makeTuples :: [(Integer, Integer)]
makeTuples = [(x,y) | x <- mySqr, y <- myCube]

makeTuples2 :: [(Integer, Integer)]
makeTuples2 = [(x,y) | x <- mySqr, y <- myCube, x < 50 && y<50]

tupleLength :: Int
tupleLength = length makeTuples2