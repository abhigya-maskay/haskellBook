import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
           (fromGregorian 1911 5 1)
           (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate dbItem = foldr f [] dbItem
  where f (DbDate x) acc = x : acc
        f _ acc = acc

filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber dbItem = foldr f [] dbItem
  where f (DbNumber x) acc = x : acc
        f _ acc =acc

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent dbItem = foldr f (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 34123)) dbItem
  where f (DbDate x) y =
          case (compare x y) of
            LT -> y
            EQ -> y
            GT -> x
        f _ y = y

sumDb :: [DatabaseItem]
      -> Integer
sumDb dbItem = foldr f 0 dbItem
  where f (DbNumber x) y = x + y
        f _ y = y

avgDb :: [DatabaseItem]
      -> Double
avgDb dbItem = (fromIntegral (fst sn)) / (fromIntegral (snd sn))
  where sn = foldr f (0,0) dbItem
        f (DbNumber x) (s,n) = (s+x,n+1)
        f _ (s,n) = (s,n)

