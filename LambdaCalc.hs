module LambdaCalc where

import Data.Maybe (fromMaybe)
import Data.List (intersperse)

data DataType = Container [DataType] | Name String | Fn [String] DataType | Unparsable deriving (Show, Eq)
data Token = BeginList | EndList | RawText String deriving (Show, Eq)

showCode (Fn params body) = concat $ ["(λ ("] ++ intersperse " " params ++ [") ", showCode body, ")"]
showCode (Name n) = n

executeText env = execute env.parse.tokenize

execute = aux
  where aux env (Name name) = fromMaybe (Name name) (lookup name env)
        aux _ (Container []) = Container []
        aux env (Container (Name "lambda":Container params:body:_)) = createLambda params body
        aux env (Container (Name "λ":Container params:body:_)) = createLambda params body
        aux env (Container (fn:rest)) = apply (aux env fn) rest env
          where apply (Fn names body) values env = let nextEnv = bind names (map (aux env) values) env in aux nextEnv body

createLambda params body = Fn (map showCode params) body

bind names = (++) . zip names

parse [] = Unparsable
parse (RawText named:_) = Name named
parse (BeginList:toParse) = Container body
  where (body, rest) = untilEndList toParse []
parse exp = error $ "Unable to parse: " ++ show exp

untilEndList (EndList:rest) acc = (reverse acc, rest)
untilEndList (BeginList:toParse) acc = untilEndList rest (Container body:acc)
  where (body, rest) = untilEndList toParse []

untilEndList r@(_:xs) acc = untilEndList xs (parse r:acc)
untilEndList [] _ = error "Unmatched ("

tokenize [] = []
tokenize ('(':ss) = BeginList:tokenize ss
tokenize (')':ss) = EndList:tokenize ss
tokenize (' ':ss) = tokenize ss
tokenize ('\n':ss) = tokenize ss
tokenize text = RawText rawText:tokenize remainder
  where rawTextAndRemainder r@(t:ts) acc =
          if t `elem` " )("
            then (reverse acc, r)
            else rawTextAndRemainder ts (t:acc)
        rawTextAndRemainder [] acc = (reverse acc, [])
        (rawText, remainder) = rawTextAndRemainder text []
