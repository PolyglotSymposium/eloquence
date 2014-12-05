module TestLisp where

import Test.Hspec
import Lisp

main :: IO ()
main = hspec $ do
  describe "executing" $ do
    describe "different forms of empty lists" $ do
      it "is an empty list, given ()" $ do
        execute [] "()" `shouldBe` AList []
      it "is an empty list, given ( )" $ do
        execute [] "( )" `shouldBe` AList []
    describe "quoted values" $ do
      it "is an empty list, given (quote ())" $ do
        execute [] "(quote ())" `shouldBe` AList []
      it "is an empty list, given '()" $ do
        execute [] "'()" `shouldBe` AList []
    describe "eq?" $ do
      it "is truthy, given 42 and 42" $ do
        execute [] "(eq? 42 42)" `shouldSatisfy` isTruthy
      it "is falsey, given 35 and 42" $ do
        execute [] "(eq? 35 42)" `shouldSatisfy` isFalsey
    describe "atom?" $ do
      it "is truthy, given 42" $ do
        execute [] "(atom? 42)" `shouldSatisfy` isTruthy
      it "is falsey, given ()" $ do
        execute [] "(atom? ())" `shouldSatisfy` isFalsey
      context "when the environment contains 'a-list' => ()" $ do
        it "is falsey, given a-list" $ do
          execute [("a-list", AList [])] "(atom? a-list)" `shouldSatisfy` isFalsey
    describe "cons" $ do
      it "is '(42 34), given 42 and (quote (35))" $ do
        execute [] "(cons 42 (quote (35)))" `shouldBe` AList [Atom "42", Atom "35"]
      it "is '(() 35), given (quote ()) (quote (35))" $ do
        execute [] "(cons (quote ()) (quote (35)))" `shouldBe` AList [AList [], Atom "35"]
    describe "tail" $ do
      it "is '(42 99), given (quote (36 42 99))" $ do
        execute [] "(tail (quote (36 42 99)))" `shouldBe` AList [Atom "42", Atom "99"]
      it "is '([] 99), given (tail (quote (36 (eq? 42 8) 99)))" $ do
        execute [] "(tail (quote (36 (eq? 42 8) 99)))" `shouldBe` AList [AList [], Atom "99"]
      it "is falsey, given ()" $ do
        execute [] "(tail ())" `shouldSatisfy` isFalsey
    describe "first" $ do
      it "is falsey, given ()" $ do
        execute [] "(first ())" `shouldSatisfy` isFalsey
      it "is 42, given (quote (42))" $ do
        execute [] "(first (quote (42)))" `shouldBe` Atom "42"
      it "is (), given (quote (quote ((eq? 42 8))))" $ do
        execute [] "(first (quote ((eq? 42 8))))" `shouldBe` AList []
    describe "cond" $ do
      context "when the environment contains a truthy thing and a falsey thing" $ do
        let env = [("truthy-thing", Atom "42"), ("falsey-thing", AList [])]
        it "is the value at the first truthy thing" $ do
          execute env "(cond truthy-thing 42)" `shouldBe` Atom "42"
        it "is falsey, given only falsey things" $ do
          execute env "(cond falsey-thing 42)" `shouldSatisfy` isFalsey
        it "is the second thing's value, given a falsey thing then a truthy thing" $ do
          execute env "(cond falsey-thing 42 truthy-thing 35)" `shouldBe` Atom "35"
    describe "simple variable lookup" $ do
      it "works for values in the environment" $ do
        execute [("foo", Atom "42")] "foo" `shouldBe` Atom "42"
    describe "lambda" $ do
      it "returns its value, when it is simple" $ do
        execute [] "((lambda () 42))" `shouldBe` Atom "42"
      it "can use its param, given one" $ do
        execute [] "((lambda (x) x) 42)" `shouldBe` Atom "42"
      it "works for multiple values" $ do
        execute [] "((lambda (x y) (cons x (cons y ()))) 1 2)" `shouldBe` AList [Atom "1", Atom "2"]
    describe "2 lists" $ do
      it "returns the result of the second one" $ do
        execute [] "() (quote 42)" `shouldBe` Atom "42"
    describe "def" $ do
      it "makes something available in later environments" $ do
        execute [] "(def foo 42) foo" `shouldBe` Atom "42"
      it "works for making then applying the identity function" $ do
        execute [] "(def identity (lambda (x) x)) (identity 42)" `shouldBe` Atom "42"
      it "works for recursion" $ do
        execute [] "(def rec (lambda (x) (cond (eq? x (quote (42))) 35 1 (rec (tail x))))) (rec (quote (1 2 3 42)))" `shouldBe` Atom "35"
      it "proves functions are closures" $ do
        execute [] "(def id (lambda (x) x)) (def x 4) (id 42)" `shouldBe` Atom "42"
    describe "+" $ do
      it "works for 1 and 1" $ do
        execute [] "(+ 1 1)" `shouldBe` Atom "2"
      it "works for environmental lookups" $ do
        execute [("foo", Atom "1"), ("bar", Atom "1")] "(+ foo bar)" `shouldBe` Atom "2"
      it "works for a variable number of arguments" $ do
        execute [] "(+ 1 2 3 4 5 6)" `shouldBe` Atom "21"
    describe "neg" $ do
      it "negates a number" $ do
        execute [] "(neg 42)" `shouldBe` Atom "-42"
      it "turns negatives, to positives" $ do
        execute [] "(neg -42)" `shouldBe` Atom "42"
      it "uses the environment" $ do
        execute [("foo", Atom "42")] "(neg foo)" `shouldBe` Atom "-42"

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

  describe "parse" $ do
    [BeginList, EndList] `shouldParseTo` AList []
    [BeginList, RawText "val", EndList] `shouldParseTo` AList [Atom "val"]
    [BeginList, RawText "a", RawText "b", EndList] `shouldParseTo` AList [Atom "a", Atom "b"]
    [RawText "val"] `shouldParseTo` Atom "val"
    [BeginList, BeginList, EndList, EndList] `shouldParseTo` AList [AList []]
    [BeginList, BeginList, RawText "bar", EndList, RawText "foo", EndList] `shouldParseTo` AList [AList [Atom "bar"], Atom "foo"]

  describe "parseMany" $ do
    it "works for 2 atoms" $ do
      parseMany [RawText "foo", RawText "bar"] `shouldBe` [Atom "foo", Atom "bar"]
    it "works for 2 lists" $ do
      parseMany [BeginList, EndList, BeginList, EndList] `shouldBe` [AList [], AList []]

isTruthy = not . isFalsey
isFalsey = (== AList [])

shouldTokenizeTo = shouldComb tokenize
shouldParseTo = shouldComb parse

shouldComb f toOpOn expected = it ("returns " ++ (show expected) ++ ", given '" ++ (show toOpOn) ++ "'") $ do
  f toOpOn `shouldBe` expected
