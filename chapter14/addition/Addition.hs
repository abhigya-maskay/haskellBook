module Addition where

import Test.Hspec
import Test.QuickCheck

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy x y = go x y 0 0
  where go x y count acc =
          case (count == y) of
            True -> acc
            False -> go x y (count + 1) (acc+x)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "5 multiplied by 3 is 15" $ do
      multiplyBy 5 3 `shouldBe` 15
    it "2 multiplied by 12 should be 24" $ do
      multiplyBy 2 12 `shouldBe` 24
    it "x + 1 is always \
       \ greater than x" $ do
      property $ \x -> x + 1 > (x::Int)
