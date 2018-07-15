isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing = x
mayybee x f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x (id)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes ((Just x):xs) = x : (catMaybes xs)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs =
  case ((length xs) == (length ys)) of
    True -> (Just ys)
    False -> Nothing
    where ys= catMaybes xs
