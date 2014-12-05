module Eloquence where

import Lisp (execute, executeText, parse, tokenize, bind, DataType(AList, Atom))
import System.IO

next env text = case (parse.tokenize) text of
  (AList [Atom "def", Atom name, v]) -> bind [name] [execute env [v]] env
  _ -> env

repl env = do
  putStr "elo> "
  hFlush stdout
  line <- getLine
  putStrLn $ show $ executeText env line
  repl $ next env line

main :: IO ()
main = repl []
