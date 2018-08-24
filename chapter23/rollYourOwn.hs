import System.Random

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1,6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0,[]) g
  where
    go :: Int -> (Int,[Die]) -> StdGen -> (Int, [Die])
    go sum acc gen
      | sum >= n = acc
      | otherwise =
        let (die, nextGen) = randomR (1,6) gen
        in go (sum + die) ((fst acc) + 1, (snd acc) ++ [intToDie die]) nextGen

main :: IO()
main = undefined
