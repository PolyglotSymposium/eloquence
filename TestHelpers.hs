module TestHelpers where

import Test.Hspec
import Lisp

shouldComb f toOpOn expected = it ("returns " ++ (show expected) ++ ", given '" ++ (show toOpOn) ++ "'") $ do
  f toOpOn `shouldBe` expected

isTruthy = not . isFalsey
isFalsey = (== AList [])
