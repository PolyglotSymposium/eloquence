module TestLisp where

import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lisp

main = runTestTT $ TestList [
  TestCase (assertEqual "exe ()" (AList []) (execute [] "()")),
  TestCase (assertEqual "exe ( )" (AList []) (execute [] "( )")),
  TestCase (assertEqual "exe (quote ())" (AList []) (execute [] "(quote ())")),
  TestCase (assertEqual "exe ('())" (AList []) (execute [] "('())")),
  TestCase (assertEqual "exe (eq? 42 42)" (AList [AList []]) (execute [] "(eq? 42 42)")),
  TestCase (assertEqual "exe (eq? 35 42)" (AList []) (execute [] "(eq? 35 42)")),
  TestCase (assertEqual "exe (atom? 42)" (Atom "42") (execute [] "(atom? 42)")),
  TestCase (assertEqual "exe (atom? (quote (42)))" (AList []) (execute [] "(atom? (quote (42)))")),
  TestCase (assertEqual "exe (atom? a-list)" (AList []) (execute [("a-list", AList [Atom "42", Atom "42"])] "(atom? a-list)")),
  TestCase (assertEqual "exe (cons 42 (quote (35)))" (AList [Atom "42", Atom "35"]) (execute [] "(cons 42 (quote (35)))")),
  TestCase (assertEqual "exe (cons (quote ()) (quote (35)))" (AList [AList [], Atom "35"]) (execute [] "(cons (quote ()) (quote (35)))")),
  TestCase (assertEqual "exe (tail (quote (35 42 99)))" (AList [Atom "42", Atom "99"]) (execute [] "(tail (quote (36 42 99)))")),
  TestCase (assertEqual "exe (tail (quote (36 (eq? 42 8) 99)))" (AList [AList [], Atom "99"]) (execute [] "(tail (quote (36 (eq? 42 8) 99)))")),
  TestCase (assertEqual "exe (tail ())" (AList []) (execute [] "(tail ())")),
  TestCase (assertEqual "exe (first ())" (AList []) (execute [] "(first ())")),
  TestCase (assertEqual "exe (first (quote (42)))" (Atom "42") (execute [] "(first (quote (42)))")),
  TestCase (assertEqual "exe (first (quote ((eq? 42 8))))" (AList []) (execute [] "(first (quote ((eq? 42 8))))")),
  TestCase (assertEqual "exe (cond truthy_thing 42)" (Atom "42") (execute [] "(cond truthy_thing 42)")),
  TestCase (assertEqual "exe (cond falsey-thing 42)" (AList []) (execute [("falsey-thing", AList [])] "(cond falsey-thing 42)")),
  TestCase (assertEqual "exe (cond falsey-thing 42 a 35)" (Atom "35") (execute [("falsey-thing", AList [])] "(cond falsey-thing 42 a 35)")),
  TestCase (assertEqual "exe (cond falsey-thing 42 falsey-thing 35)" (AList []) (execute [("falsey-thing", AList [])] "(cond falsey-thing 42 falsey-thing 35)")),
  TestCase (assertEqual "exe(bound) foo" (Atom "42") (execute [("foo", Atom "42")] "foo")),
  TestCase (assertEqual "exe simple lambda" (Atom "42") (execute [] "((lambda () 42))")),
  TestCase (assertEqual "exe identity lambda" (Atom "42") (execute [] "((lambda (x) x) 42)")),
  TestCase (assertEqual "exe foo" (Atom "42") (execute [("foo", Atom "42")] "foo")),
  TestCase (assertEqual "exe 2 lists" (Atom "42") (execute [] "()(' 42)")),
  TestCase (assertEqual "tokenize (" [BeginList] (tokenize "(")),
  TestCase (assertEqual "tokenize )" [EndList] (tokenize ")")),
  TestCase (assertEqual "tokenize <space>" [] (tokenize " ")),
  TestCase (assertEqual "tokenize ()" [BeginList, EndList] (tokenize "()")),
  TestCase (assertEqual "tokenize )(" [EndList, BeginList] (tokenize ")(")),
  TestCase (assertEqual "tokenize (quote foobar)" [BeginList, RawText "quote", RawText "foobar", EndList] (tokenize "(quote foobar)")),
  TestCase (assertEqual "parse [BList, EList]" (AList []) (parse [BeginList, EndList])),
  TestCase (assertEqual "parse [BeginList, RawText 'val', EndList]" (AList [Atom "val"]) (parse [BeginList, RawText "val", EndList])),
  TestCase (assertEqual "parse two vals in a list" (AList [Atom "a", Atom "b"]) (parse [BeginList, RawText "a", RawText "b", EndList])),
  TestCase (assertEqual "parse [RawText 'val']" (Atom "val") (parse [RawText "val"])),
  TestCase (assertEqual "parse List within" (AList [AList []]) (parse [BeginList, BeginList, EndList, EndList])),
  TestCase (assertEqual "parse complex nesting" (AList [AList [Atom "bar"], Atom "foo"]) (parse [BeginList, BeginList, RawText "bar", EndList, RawText "foo", EndList])),
  TestCase (assertEqual "parseMany 2 atoms" [Atom "foo", Atom "bar"] (parseMany [RawText "foo", RawText "bar"])),
  TestCase (assertEqual "parseMany 2 lists" [AList [], AList []] (parseMany [BeginList, EndList, BeginList, EndList])),
  TestCase (assertEqual "tokenize a" [RawText "a"] (tokenize "a")),
  TestCase (assertEqual "tokenize foobar" [RawText "foobar"] (tokenize "foobar")),
  TestCase (assertEqual "tokenize b" [RawText "b"] (tokenize "b")),
  TestCase (assertEqual "tokenize nexted" [BeginList, RawText "empty?", BeginList, EndList, EndList] (tokenize "(empty? ())"))]
