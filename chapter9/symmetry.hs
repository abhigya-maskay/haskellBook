module PoemLines where

splitString :: Char -> String -> [String]
splitString _ "" = []
splitString c s = takeWhile (/= c) s : splitString c (drop 1 . dropWhile (/= c) $ s)

myWords :: String -> [String]
myWords = splitString ' '

firstSen = "Tyger Tyger, burning bright \n"
secondSen = "In the forests of the night \n"
thirdSen = "What immortal hand or eye \n"
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = splitString '\n' 

shouldEqual :: [String]
shouldEqual = ["Tyger Tyger, burning bright ", "In the forests of the night ", "What immortal hand or eye ", "Could frame thy fearful symmetry?"]

main :: IO()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)

