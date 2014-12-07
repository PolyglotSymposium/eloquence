module TestParser where

import Test.Hspec
import Lisp
import TestHelpers

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    [BeginList, EndList] `shouldParseTo` AList []
    [BeginList, RawText "val", EndList] `shouldParseTo` AList [Atom "val"]
    [BeginList, RawText "a", RawText "b", EndList] `shouldParseTo` AList [Atom "a", Atom "b"]
    [RawText "val"] `shouldParseTo` Atom "val"
    [BeginList, BeginList, EndList, EndList] `shouldParseTo` AList [AList []]
    [BeginList, BeginList, RawText "bar", EndList, RawText "foo", EndList] `shouldParseTo` AList [AList [Atom "bar"], Atom "foo"]
    [] `shouldParseTo` Unparsable

  describe "parseMany" $ do
    it "works for 2 atoms" $ do
      parseMany [RawText "foo", RawText "bar"] `shouldBe` [Atom "foo", Atom "bar"]
    it "works for 2 lists" $ do
      parseMany [BeginList, EndList, BeginList, EndList] `shouldBe` [AList [], AList []]

shouldParseTo = shouldComb parse
