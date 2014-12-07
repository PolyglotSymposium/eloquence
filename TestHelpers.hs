module TestHelpers where

import Test.Hspec

shouldComb f toOpOn expected = it ("returns " ++ (show expected) ++ ", given '" ++ (show toOpOn) ++ "'") $ do
  f toOpOn `shouldBe` expected
