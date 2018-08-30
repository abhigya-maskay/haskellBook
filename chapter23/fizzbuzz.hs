import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo m n
  | m == n = fizzBuzzList [m]
  | m < n && n-m == 1 = fizzBuzzList [n,m]
  | m < n = fizzBuzzList [n, n-1 .. m]
  | otherwise = fizzBuzzFromTo n m

main :: IO()
main = mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]
