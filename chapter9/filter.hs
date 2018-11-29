import Data.List

filterMult3 :: [Integer]
filterMult3 = filter (\x -> x `mod` 3 == 0) [1..30]

filterMult3Length :: Int
filterMult3Length = length filterMult3

filterArticles :: String -> [String]
filterArticles = filter (\x -> not $ x `elem` ["a", "an", "the"]) . words