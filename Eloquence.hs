module Main where

import Lisp (execute, executeText, parse, tokenize, bind, DataType(AList, Atom))
import System.IO

next env text = case (parse.tokenize) text of
  (AList [Atom "def", Atom name, v]) -> (name, execute env [v]):env
  _ -> env

repl env = do
  putStr "elo> "
  hFlush stdout
  line <- getLine
  let executedLine = executeText env line
  putStrLn $ show $ executedLine
  repl $ (("it", executedLine):next env line)

main :: IO ()
main = repl []
