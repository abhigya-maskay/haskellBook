{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

  import Control.Applicative
  import Text.Trifecta
  import Data.List
  import Data.ByteString (ByteString)
  import Text.RawString.QQ
  import Data.Maybe
  import qualified Data.Map as M 

  leftPad :: Integer -> String -> String
  leftPad n s =  (take (fromIntegral (n - (genericLength s))) $ repeat '0') ++ s

  type Comment = String

  data DayLog = DayLog Date [Entry] deriving (Eq)
  instance Show DayLog where
    show (DayLog date (x:xs)) = show date ++ "\n" ++ showEntries (x:xs) where
      showEntries (x: []) = show x
      showEntries (x: xs) = show x ++ "\n" ++ showEntries xs

  data Date = Date Year Month Day deriving (Eq)
  instance Show Date where
    show (Date year month day) = "# " ++ yearString ++ "-" ++ monthString ++ "-" ++ dayString where
      yearString = leftPad 4 $ show year
      monthString = leftPad 2 $ show month
      dayString = leftPad 2 $ show day

  type Year = Integer
  type Month = Integer
  type Day = Integer

  data Entry = Entry Time Activity deriving (Eq)
  instance Show Entry where
    show (Entry time activity) = show time ++ " " ++ activity

  type Activity = String

  data Time = Time Hour Minute deriving (Eq)
  instance Show Time where
    show (Time hour minute) = hourString ++ ":" ++ minuteString where
      hourString = leftPad 2 $ show hour
      minuteString = leftPad 2 $ show minute

  type Hour = Integer
  type Minute = Integer

  data Log = Log [DayLog]
  instance Show Log where
    show (Log (x:xs)) = showLogs (x:xs) where
      showLogs (x:[]) = show x
      showLogs (x:xs) = show x ++ "\n" ++ showLogs xs
  
  logEx :: ByteString
  logEx = [r|
    -- wheee a comment

    # 2025-02-05
    08:00 Breakfast
    09:00 Sanitizing moisture collector
    11:00 Exercising in high-grav gym
    12:00 Lunch
    13:00 Programming
    17:00 Commuting home in rover
    17:30 R&R
    19:00 Dinner
    21:00 Shower
    21:15 Read
    22:00 Sleep
    
    # 2025-02-07 -- dates not nececessarily sequential
    08:00 Breakfast -- should I try skippin bfast?
    09:00 Bumped head, passed out
    13:36 Wake up, headache
    13:37 Go to medbay
    13:40 Patch self up
    13:45 Commute home for rest
    14:15 Read
    21:00 Dinner
    21:15 Read
    22:00 Sleep
  |]

  comment :: Parser String
  comment = try (someSpace >> string "--") <|> string "--"

  skipLine :: Parser ()
  skipLine = do 
    skipMany (noneOf "\n") 
    skipMany (oneOf "\n")

  skipSpaces :: Parser ()
  skipSpaces = do
    skipMany (char ' ') 
  
  skipWhitespace :: Parser ()
  skipWhitespace = skipOptional $ skipMany (char ' ' <|> char '\n')

  skipComments :: Parser ()
  skipComments = skipOptional $ comment >> skipMany (noneOf "\n") >> skipWhitespace

  parseDate :: Parser Date
  parseDate = do
    skipOptional skipWhitespace
    skipMany (oneOf "#")
    skipOptional $ skipMany (char ' ')
    a <- (read <$> count 4 digit) :: Parser Integer
    char '-'
    b <- (read <$> count 2 digit) :: Parser Integer
    char '-'
    c <- (read <$> count 2 digit) :: Parser Integer
    skipComments 
    skipWhitespace
    return $ Date a b c

  parseTime :: Parser Time
  parseTime = do
    skipOptional skipSpaces
    a <- (read <$> count 2 digit) :: Parser Integer
    char ':'
    b <- (read <$> count 2 digit) :: Parser Integer
    return $ Time a b

  parseActivity :: Parser String
  parseActivity = do
    skipOptional skipSpaces
    a <- try (manyTill (noneOf "\n") comment) <|> many (noneOf "\n")
    skipLine
    return a

  parseEntry :: Parser Entry
  parseEntry = do
    a <- parseTime
    b <- parseActivity
    skipWhitespace
    return $ Entry a b

  parseDayLog :: Parser DayLog
  parseDayLog = do
    skipComments
    skipWhitespace
    a <- parseDate
    b <- some parseEntry
    return $ DayLog a b

  parseLog :: Parser [DayLog]
  parseLog = do
    dayLogs <- some parseDayLog
    return dayLogs

  getDayLogs :: Result [DayLog] -> Maybe [DayLog]
  getDayLogs (Success a) = Just a
  getDayLogs _ = Nothing

  parsedLogEx :: [DayLog]
  parsedLogEx = fromJust . getDayLogs $ parseByteString parseLog mempty logEx

  timeDifference :: Time -> Time -> Time
  timeDifference (Time h1 m1) (Time h2 m2) = Time (minutesDifference `quot` 60) (minutesDifference `rem` 60) where
    minutesDifference = ((h2*60 + m2) - (h1*60 + m1))
  
  timeTotal :: Time -> Time -> Time
  timeTotal (Time h1 m1) (Time h2 m2) = Time (totalMinutes `quot` 60) (totalMinutes `rem` 60) where
    totalMinutes = ((h1 * 60 + m1) + (h2 * 60 + m2))

  differenceEntries :: Entry -> Entry -> Entry
  differenceEntries (Entry a1 b1) (Entry a2 b2) = Entry (timeDifference a1 a2) b1

  addEntries :: Entry -> Entry -> Entry
  addEntries (Entry a1 b1) (Entry a2 b2) = Entry (timeTotal a1 a2) b1

  timeEntries :: [Entry] -> [Entry]
  timeEntries ((Entry a1 b1): []) = [Entry (Time 0 0) b1]
  timeEntries (x:xs) = differenceEntries x (head xs) : timeEntries (xs)

  timedDayLog :: DayLog -> DayLog
  timedDayLog (DayLog date entries) = DayLog date (timeEntries entries)

  timedLog :: [DayLog] -> [DayLog]
  timedLog = fmap timedDayLog

  foldDayLog :: M.Map Activity Time -> DayLog -> M.Map Activity Time
  foldDayLog m (DayLog date entries) = rollup m entries where
    rollup :: M.Map Activity Time -> [Entry] -> M.Map Activity Time
    rollup m entries = foldr (\(Entry t a) m -> M.insertWith timeTotal a t m) M.empty entries

  foldLog :: M.Map Activity Time -> [DayLog] -> M.Map Activity Time
  foldLog m daylogs = foldr (M.unionWith timeTotal) m (fmap (foldDayLog m) daylogs)

  main :: IO()
  main = undefined