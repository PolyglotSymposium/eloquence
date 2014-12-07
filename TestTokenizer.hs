module TestTokenizer where

import Test.Hspec
import Lisp
import TestHelpers

main = hspec $ do
  describe "tokenize" $ do
    "(" `shouldTokenizeTo` [BeginList]
    ")" `shouldTokenizeTo` [EndList]
    " " `shouldTokenizeTo` []
    "()" `shouldTokenizeTo` [BeginList, EndList]
    ")(" `shouldTokenizeTo` [EndList, BeginList]
    ")(" `shouldTokenizeTo` [EndList, BeginList]
    "(quote foobar)" `shouldTokenizeTo` [BeginList, RawText "quote", RawText "foobar", EndList]
    "a" `shouldTokenizeTo` [RawText "a"]
    "foobar" `shouldTokenizeTo` [RawText "foobar"]
    "(empty? ())" `shouldTokenizeTo` [BeginList, RawText "empty?", BeginList, EndList, EndList]

shouldTokenizeTo = shouldComb tokenize
