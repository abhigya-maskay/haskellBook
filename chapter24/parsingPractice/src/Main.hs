module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one = eof >> char '1'

one' = one >> stop

oneTwo = eof >> char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO()
testParse p = print $ parseString p mempty "123"

stringParse :: Parser String -> IO()
stringParse p = print $ parseString p mempty "123"

allThree :: String -> Parser String
allThree s = string s

charAllThree :: String -> Parser String
charAllThree s = go s mempty where
  go (x:[]) acc = return (acc <> pure x)
  go (x:xs) acc = go xs (acc <> pure x)

pNL s = putStrLn ('\n' : s)

main :: IO ()
main = do
  pNL "Stop:"
  testParse stop
  pNL "One:"
  testParse one
  pNL "One':"
  testParse one'
  pNL "OneTwo:"
  testParse oneTwo
  pNL "OneTwo':"
  testParse oneTwo'
  pNL "String 1:"
  stringParse $ allThree "1"
  pNL "String 2:"
  stringParse $ allThree "12"
  pNL "String 3:"
  stringParse $ allThree "123"
  pNL "CharString 1:"
  stringParse $ charAllThree "1"
  pNL "CharString 2:"
  stringParse $ charAllThree "12"
  pNL "CharString 3:"
  stringParse $ charAllThree "123"
