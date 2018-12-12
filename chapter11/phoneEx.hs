import           Data.Char
import           Data.Function
import           Data.List

newtype DaPhone = DaPhone [Button]

data Button = Button Char String

phone :: DaPhone
phone = DaPhone [ Button '1' "1"
                , Button '2' "ABC2"
                , Button '3' "DEF3"
                , Button '4' "GHI4"
                , Button '5' "JKL5"
                , Button '6' "MNO6"
                , Button '7' "PQRS7"
                , Button '8' "TUV8"
                , Button '9' "WXYZ9"
                , Button '*' "^*"
                , Button '0' " 0"
                , Button '#' ".,"]

convo :: [String]
convo = ["Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have you ever tasted alcohol"
        , "Lol ya"
        , "Wow ur cool haha.Ur turn"
        , "Ok. Do you think I am pretty Lol"
        , "Lol ya"
        , "Just making sure rofl ur turn"]

type Digit = Char
type Presses = Int


reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone (x:xs)) char = if isUpper char
                                       then ('*',1):reverseTap (DaPhone (x:xs)) char
                                       else reverseTap (DaPhone (x:xs)) char where
                                         reverseTap :: DaPhone -> Char -> [(Digit, Presses)]
                                         reverseTap (DaPhone []) _ = []
                                         reverseTap (DaPhone (b:bs)) c = if includeChar b c
                                                         then [getTaps b c]
                                                         else reverseTap (DaPhone bs) c

includeChar :: Button -> Char -> Bool
includeChar (Button _ chars) char = toUpper char `elem` chars

getTaps :: Button -> Char -> (Digit, Presses)
getTaps button char = go button char 1 where
  go :: Button -> Char -> Int -> (Digit, Presses)
  go (Button digit []) _ _ = (digit, 0)
  go (Button digit (x:xs)) char1 counter = if toUpper char1 == x
                                             then (digit, counter)
                                             else go (Button digit xs) char1 (counter + 1)

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (reverseTaps p)

reverseConvo :: DaPhone -> [String] -> [[(Digit, Presses)]]
reverseConvo p = map (cellPhonesDead p)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(d1,p1) p2 -> p1+p2) 0

mostPopularLetter :: String -> Char
mostPopularLetter = head . maximumBy (compare `on` length) . group . sort

mostPopularLetterCost :: String -> Presses
mostPopularLetterCost s = fingerTaps . filter mostPopular $ digitPresses where
  digitPresses :: [(Digit, Presses)]
  digitPresses = cellPhonesDead phone s

  mostPopular :: (Digit, Presses) -> Bool
  mostPopular (d1,p1) = (d1,p1) == if length tap == 1 then head tap else head . tail $ tap where
    tap = reverseTaps phone (mostPopularLetter s)

coolestLtr :: [String] -> Char
coolestLtr = head . maximumBy (compare `on` length) . group . sort . filter isAlpha . concat

coolestWord :: [String] -> String
coolestWord = head . maximumBy (compare `on` length) . group . sort . concatMap words

main :: IO()
main = undefined
