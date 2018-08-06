module FixerUpper where

constFix = const <$> Just "Hello" <*> pure "World"

justFix = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]

main :: IO()
main = undefined
