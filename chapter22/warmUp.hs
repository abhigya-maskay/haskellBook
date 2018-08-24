import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  a <- cap
  b <- rev
  return $ (,) a b

tupledBind :: [Char] -> ([Char], [Char])
tupledBind = cap >>= (\x1 -> rev >>= (\x2 -> return $ (x1,x2)))
