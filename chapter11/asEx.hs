import Data.Char

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs@(x:xtail) ys@(y:ytail) =
  case (x `elem` ys) of
    True -> isSubseqOf xtail ytail
    False -> isSubseqOf xs ytail

capitalizeWords :: String
                -> [(String,String)]
capitalizeWords [] = [("","")]
capitalizeWords xs = map capitalizeFirst $ words xs
  where capitalizeFirst xs@(x:xtail) = ((toUpper x : xtail), xs)
