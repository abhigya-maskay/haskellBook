import           Data.Array

--asldjsalkdjsaldkjsaldkj
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe def f Nothing  = def
mayybe def f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe def = mayybe def id

listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe []     = Nothing

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing  = []

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
  Nothing -> catMaybes xs
  Just a  -> a : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (x:xs) = case x of
  Nothing -> Nothing
  Just a  -> (a:) <$> flipMaybe xs
