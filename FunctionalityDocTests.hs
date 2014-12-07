module FunctionalityDocTests where

import Test.Hspec
import Lisp
import TestHelpers

main :: IO ()
main = hspec $ do
  describe "# The Eloquence Language" $ do
    describe "## List Basics" $ do
    describe "### `()`" $ do
      it "is the empty list." $ do
        executeText [] "()" `shouldBe` AList []
    describe "### `quote`" $ do
      describe "is a function used to forego evaluation." $ do
        describe "For example, `(quote (1 2 3 4))`" $ do
          let described = executeText [] "(quote (1 2 3 4))"
          it "is like saying a list holding `1`, `2`, `3` and `4`." $ do
            described `shouldBe` AList [Atom "1", Atom "2", Atom "3", Atom "4"]
    describe "### `cons`" $ do
      describe "prepends an element to a list." $ do
        describe "For example, `(cons 42 ())`" $ do
          let described = executeText [] "(cons 42 ())"
          it "is `(42)`." $ do
            described `shouldBe` AList [Atom "42"]
      describe "Remember, lists are values too!" $ do
        describe "So, `(cons () ())`" $ do
          let described = executeText [] "(cons () ())"
          it "is `(())`." $ do
            described `shouldBe` AList [AList []]
    describe "### `tail`" $ do
      describe "returns a list containing all but the first element" $ do
        describe "so, `(tail (quote (1 2 3 4)))`" $ do
          let described = executeText [] "(tail (quote (1 2 3 4)))"
          it "is `(2 3 4)`." $ do
            described `shouldBe` AList [Atom "2", Atom "3", Atom "4"]
        describe "if an empty list is given to `tail`" $ do
          let described = executeText [] "(tail ())"
          it "an empty list is returned." $ do
            described `shouldBe` AList []
    describe "### `first`" $ do
      describe "returns the first element of a list." $ do
        describe "For example, `(first (quote (1 2 3 4 5)))`" $ do
          let described = executeText [] "(first (quote (1 2 3 4 5)))"
          it "is `1`." $ do
            described `shouldBe` Atom "1"
        describe "if an empty list is given to `first`" $ do
          let described = executeText [] "(first ())"
          it "an empty list is returned." $ do
            described `shouldBe` AList []
  describe "## Checking Values" $ do
    describe "### `atom?`" $ do
      describe "is truthy, when given anything that is not a list." $ do
        describe "For example, `(atom? 42)`" $ do
          let described = executeText [] "(atom? 42)"
          it "is truthy." $ do
            described `shouldSatisfy` isTruthy
      describe "It returns the falsey value (aka the empty list), when given a list." $ do
        describe "For example, `(atom? ())`" $ do
          let described = executeText [] "(atom? ())"
          it "is falsey." $ do
            described `shouldSatisfy` isFalsey
    describe "### `eq?`" $ do
      describe "is truthy, when given two of the same thing." $ do
        describe "For example, `(eq? 1 1)`" $ do
          let described = executeText [] "(eq? 1 1)"
          it "is truthy." $ do
            described `shouldSatisfy` isTruthy
      describe "Is falsey, when given two different things." $ do
        describe "For example, `(eq? 42 ())`" $ do
          let described = executeText [] "(eq? 42 ())"
          it "is falsey." $ do
            described `shouldSatisfy` isFalsey
      describe "`eq?` Performs a by-value compare" $ do
        describe "so, `(eq? (quote (1 2 3)) (quote (1 2 3)))`" $ do
          let described = executeText [] "(eq? (quote (1 2 3)) (quote (1 2 3)))"
          it "is truthy." $ do
            described `shouldSatisfy` isTruthy
    describe "### AND MORE" $ do
      it "docs are a work in progress :)" $ do
        pending
