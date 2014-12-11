module TestLisp where

import Test.Hspec
import Lisp
import TestHelpers

main :: IO ()
main = hspec $ do
  describe "execution" $ do
    describe "different forms of empty lists" $ do
      it "is an empty list, given ( )" $ do
        executeText [] "( )" `shouldBe` AList []
    describe "quoted values" $ do
      it "is an empty list, given (quote ())" $ do
        executeText [] "(quote ())" `shouldBe` AList []
      it "is an empty list, given '()" $ do
        executeText [] "'()" `shouldBe` AList []
    describe "eq?" $ do
      it "is truthy, given 42 and 42" $ do
        executeText [] "(eq? 42 42)" `shouldSatisfy` isTruthy
      it "is falsey, given 35 and 42" $ do
        executeText [] "(eq? 35 42)" `shouldSatisfy` isFalsey
    describe "atom?" $ do
      context "when the environment contains 'a-list' => ()" $ do
        it "is falsey, given a-list" $ do
          executeText [("a-list", AList [])] "(atom? a-list)" `shouldSatisfy` isFalsey
    describe "cons" $ do
      it "is '(42 34), given 42 and (quote (35))" $ do
        executeText [] "(cons 42 (quote (35)))" `shouldBe` AList [Atom "42", Atom "35"]
      it "is '(() 35), given (quote ()) (quote (35))" $ do
        executeText [] "(cons (quote ()) (quote (35)))" `shouldBe` AList [AList [], Atom "35"]
    describe "tail" $ do
      it "is '(42 99), given (quote (36 42 99))" $ do
        executeText [] "(tail (quote (36 42 99)))" `shouldBe` AList [Atom "42", Atom "99"]
      it "is '([] 99), given (tail (quote (36 (eq? 42 8) 99)))" $ do
        executeText [] "(tail (quote (36 (eq? 42 8) 99)))" `shouldBe` AList [AList [], Atom "99"]
      it "is falsey, given ()" $ do
        executeText [] "(tail ())" `shouldSatisfy` isFalsey
    describe "first" $ do
      it "is falsey, given ()" $ do
        executeText [] "(first ())" `shouldSatisfy` isFalsey
      it "is 42, given (quote (42))" $ do
        executeText [] "(first (quote (42)))" `shouldBe` Atom "42"
      it "is (), given (quote (quote ((eq? 42 8))))" $ do
        executeText [] "(first (quote ((eq? 42 8))))" `shouldBe` AList []
    describe "cond" $ do
      context "when the environment contains a truthy thing and a falsey thing" $ do
        let env = [("truthy-thing", Atom "42"), ("falsey-thing", AList [])]
        it "is the value at the first truthy thing" $ do
          executeText env "(cond truthy-thing 42)" `shouldBe` Atom "42"
        it "is falsey, given only falsey things" $ do
          executeText env "(cond falsey-thing 42)" `shouldSatisfy` isFalsey
        it "is the second thing's value, given a falsey thing then a truthy thing" $ do
          executeText env "(cond falsey-thing 42 truthy-thing 35)" `shouldBe` Atom "35"
    describe "simple variable lookup" $ do
      it "works for values in the environment" $ do
        executeText [("foo", Atom "42")] "foo" `shouldBe` Atom "42"
    describe "lambda" $ do
      it "returns its value, when it is simple" $ do
        executeText [] "((lambda () 42))" `shouldBe` Atom "42"
      it "can use its param, given one" $ do
        executeText [] "((lambda (x) x) 42)" `shouldBe` Atom "42"
      it "works for multiple values" $ do
        executeText [] "((lambda (x y) (cons x (cons y ()))) 1 2)" `shouldBe` AList [Atom "1", Atom "2"]
      it "can accept and apply other lambdas" $ do
        executeText [] "((lambda (x) (x)) (lambda () 42))" `shouldBe` Atom "42"
    describe "2 lists" $ do
      it "returns the result of the second one" $ do
        executeText [] "() (quote 42)" `shouldBe` Atom "42"
    describe "def" $ do
      it "makes something available in later environments" $ do
        executeText [] "(def foo 42) foo" `shouldBe` Atom "42"
      it "works for making then applying the identity function" $ do
        executeText [] "(def identity (lambda (x) x)) (identity 42)" `shouldBe` Atom "42"
      it "works for recursion" $ do
        executeText [] "(def rec (lambda (x) (cond (eq? x (quote (42))) 35 1 (rec (tail x))))) (rec (quote (1 2 3 42)))" `shouldBe` Atom "35"
      it "proves functions are closures" $ do
        executeText [] "(def id (lambda (x) x)) (def x 4) (id 42)" `shouldBe` Atom "42"
    describe "+" $ do
      it "works for 1 and 1" $ do
        executeText [] "(+ 1 1)" `shouldBe` Atom "2"
      it "works for environmental lookups" $ do
        executeText [("foo", Atom "1"), ("bar", Atom "1")] "(+ foo bar)" `shouldBe` Atom "2"
      it "works for a variable number of arguments" $ do
        executeText [] "(+ 1 2 3 4 5 6)" `shouldBe` Atom "21"
    describe "-" $ do
      it "works for 42 and 2" $ do
        executeText [] "(- 42 2)" `shouldBe` Atom "40"
      it "works for environmental lookups" $ do
        executeText [("foo", Atom "42"), ("bar", Atom "2")] "(- foo bar)" `shouldBe` Atom "40"
      it "works for a variable number of arguments" $ do
        executeText [] "(- 99 2 3 4 5 6)" `shouldBe` Atom "79"
    describe "neg" $ do
      it "negates a number" $ do
        executeText [] "(neg 42)" `shouldBe` Atom "-42"
      it "turns negatives, to positives" $ do
        executeText [] "(neg -42)" `shouldBe` Atom "42"
      it "uses the environment" $ do
        executeText [("foo", Atom "42")] "(neg foo)" `shouldBe` Atom "-42"
    describe "if" $ do
      it "returns the 2nd arg if truthy" $ do
        executeText [] "(if 1 42 37)" `shouldBe` Atom "42"
      it "returns the 3rd arg if falsey" $ do
        executeText [] "(if () 42 37)" `shouldBe` Atom "37"
      it "does env lookups" $ do
        executeText [("f", AList []), ("p", Atom "66")] "(if f 42 p)" `shouldBe` Atom "66"
      it "does not evaluate expressions that will not be returned" $ do
        executeText [] "(if truthy-thing 42 (i-will-break-if-hit))" `shouldBe` Atom "42"
    describe "let" $ do
      it "makes a 'local' variable" $ do
        executeText [] "(let (x 42) x)" `shouldBe` Atom "42"
      it "nested shadow" $ do
        executeText [] "(let (x 42) (let (x 33) x))" `shouldBe` Atom "33"
      it "multiple pairs of args can be used" $ do
        executeText [] "(let (x 42 y 33) (+ x y))" `shouldBe` Atom "75"
      it "future vars can be defined in terms of prior" $ do
        executeText [] "(let (x 42 y (+ x 33)) y)" `shouldBe` Atom "75"
  describe "passing around primitive names" $ do
    it "works" $ do
      executeText [] "(def a (lambda (f x) (f x))) (atom? 42)" `shouldSatisfy` isTruthy
