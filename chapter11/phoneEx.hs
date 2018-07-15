import Data.Char

data Button = Button {
  digit :: Char
  ,chars :: String
  }
  deriving (Eq,Show)
data DaPhone = DaPhone [Button] deriving (Show)

phone :: DaPhone
phone = DaPhone [Button '1' "1"
                ,Button '2' "abc2"
                ,Button '3' "def3"
                ,Button '4' "ghi4"
                ,Button '5' "jkl6"
                ,Button '6' "mno6"
                ,Button '7' "pqrs7"
                ,Button '8' "tuv8"
                ,Button '9' "wxyz9"
                ,Button '*' ""
                ,Button '#' ".,"
                ,Button '0' " 0"
                ]

convo :: [String]
convo =
  ["Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have you ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do you think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

type Digit = Char
type Presses = Int

containsChar :: Char -> Button -> Bool
containsChar s (Button y ys)= s `elem` ys

getPresses :: Char -> Button -> Int
getPresses s (Button x (y:ys)) = go s (y:ys) 0
  where go s (y:ys) acc =
          case (s == y) of
            True -> acc + 1
            False -> go s ys (acc+1)

getButton :: Char -> DaPhone -> Button
getButton s (DaPhone phone) = head . filter (containsChar s) $ phone

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps phone s = go phone s []
  where go phone s acc =
          case (isUpper s) of
            True -> go phone (toLower s) [('*',1)]
            False -> acc ++ [(digit button, getPresses s button)]
              where button = getButton s phone

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone [] = []
cellPhonesDead phone s = foldr ((++) . (reverseTaps phone)) [] s

convertConvo :: DaPhone -> [String] -> [[(Digit, Presses)]]
convertConvo phone convo = map (cellPhonesDead phone) convo

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps x = foldr ((+) . snd) 0 x

occurance :: String -> Char -> Int
occurance [] _ = 0
occurance s c = foldr (\a b -> if a==c then b+1 else b) 0 s

mostPopularLetter :: String -> Char
mostPopularLetter s@(x:xs) = foldr (\a b -> if (occurance s a) > (occurance s b) then a else b) x xs

letterCost :: Char -> Presses
letterCost = fingerTaps . (reverseTaps phone)

mostPopularLetterCost :: String -> Int
mostPopularLetterCost = letterCost . mostPopularLetter

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . (foldr (++) [])

wordOccurance :: [String] -> String -> Int
wordOccurance [] _ = 0
wordOccurance sentence word = foldr (\a b -> if a == word then b+1 else b) 0 sentence

mostPopularWord :: [String] -> String
mostPopularWord s@(x:xs) = foldr (\a b -> if (wordOccurance s a) > (wordOccurance s b) then a else b) x xs

coolestWord :: [String] -> String
coolestWord = mostPopularWord . words . (foldr (++) [])
