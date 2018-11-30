import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello, world!"
              , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
              , DbNumber 1]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate items =  map getUTC $ filter isDbDate items where
  isDbDate item = case item of
    DbDate _ -> True
    _ -> False
  
  getUTC (DbDate utc) = utc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber items = map getNumber $ filter isDbNumber items where
  isDbNumber item = case item of
    DbNumber _ -> True
    _ -> False

  getNumber (DbNumber number) = number

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent list = foldr max (head filtered) (tail filtered) where
  max = \a b -> case (compare a b) of
    GT -> a
    _ -> b
  
  filtered = filterDbDate list

sumDb :: [DatabaseItem] -> Integer
sumDb list = foldr (+) 0 $ filterDbNumber list

avgDb :: [DatabaseItem] -> Double
avgDb list = (fromIntegral $ sumDb list) / (fromIntegral . length . filterDbNumber $ list)