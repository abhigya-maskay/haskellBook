import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a,b,c)]
combos a b c = liftA3 (,,) a b c

main = do
  print (combos stops vowels stops)
