module Main where
import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Test.Hspec

newtype WordList = WordList [String]
  deriving (Eq,Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

gameWords :: IO WordList
gameWords = do
  WordList aw <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in   l >= minWordLength
            && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, ((length wl) - 1))
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

instance Eq Puzzle where
  (Puzzle x1 x2 x3) == (Puzzle y1 y2 y3) = x1 == y1 && x2 == y2 && x3 == y3

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle x _ _) y = y `elem` x

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ x) y = y `elem` x

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just x)= x

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c:s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed then Just wordChar else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick \
               \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
               \ word, filling in the word\
               \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
               \ the word, try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when ((length guessedWrong) > 7) $
    do putStrLn "You Lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
    where guessedWrong = filter (\s -> not $ s `elem` wordToGuess) guessed

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $
    do putStrLn "You win!"
       exitSuccess

runGame :: Puzzle -> IO()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must\
                 \ be a single character"

test :: IO ()
test = hspec $ do
  describe "fill in character" $ do
    it "Given a correct guess it fills in the word properly" $ do
      (fillInCharacter
        (Puzzle "apple" [Nothing, Just 'p', Just 'p', Nothing, Nothing] "p")
        'a')
      `shouldBe` (Puzzle "apple" [Just 'a', Just 'p', Just 'p', Nothing, Nothing] "ap")
    it "Given a correct guess and empty guessed word fill in properly" $ do
      (fillInCharacter
       (Puzzle "apple" [Nothing,Nothing,Nothing,Nothing,Nothing] "")
       'a')
        `shouldBe` (Puzzle "apple" [Just 'a', Nothing, Nothing, Nothing, Nothing] "a")
    it "Given a repeating guess, fills in the word properly" $ do
      (fillInCharacter
       (Puzzle "apple" [Nothing, Nothing, Nothing, Nothing, Nothing] "")
       'p')
        `shouldBe` (Puzzle "apple" [Nothing, Just 'p', Just 'p', Nothing, Nothing] "p")
    it "Fills in the last remaining character properly" $ do
      (fillInCharacter
        (Puzzle "apple" [Nothing, Just 'p', Just 'p', Just 'l', Just 'e'] "ple")
        'a')
        `shouldBe` (Puzzle "apple" [Just 'a', Just 'p', Just 'p', Just 'l', Just 'e'] "aple")
    it "Does not fill in the word given an invalid guess" $ do
      (fillInCharacter
        (Puzzle "apple" [Nothing, Nothing, Nothing, Nothing, Nothing] "")
        'x')
      `shouldBe` (Puzzle "apple" [Nothing,Nothing,Nothing,Nothing,Nothing] "x")

main :: IO ()
main = do
  test
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
