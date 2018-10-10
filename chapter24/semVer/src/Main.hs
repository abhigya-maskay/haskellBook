{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE QuasiQuotes#-}

module Main where

import Control.Applicative
import Data.Text (Text)
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq)
instance Ord NumberOrString where
  compare (NOSI _) (NOSS _) = LT
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI x) (NOSI y) = compare x y
  compare (NOSS x) (NOSS y) = compare x y

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)
instance Ord SemVer where
  compare (SemVer ma1 mi1 p1 r1 _) (SemVer ma2 mi2 p2 r2 _) =
    compare ma1 ma2
    <> compare mi1 mi2
    <> compare p1 p2
    <> compare r1 r2

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = skipMany (oneOf ".") >> NOSI <$> try (decimal <* notFollowedBy letter) <|> NOSS <$> some (alphaNum)


parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal
  release <- char '-' *> some parseNumberOrString <|> mempty
  metadata <- char '+' *> some parseNumberOrString <|> mempty
  return $ SemVer major minor patch release metadata

main :: IO ()
main = do
  putStrLn "hello world"
