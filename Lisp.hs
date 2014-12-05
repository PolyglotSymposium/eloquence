module Lisp where

type ExecutionEnvironment = [(String, DataType)]

type Macro = (DataType -> DataType) -> [DataType] -> DataType

data DataType = AList [DataType] | Atom String | Fn [String] DataType deriving (Show, Eq)
data Token = BeginList | EndList | RawText String deriving (Show, Eq)

execute env code = executeLevel env asts
  where executeLevel env (AList [Atom "def", Atom name, value]:rest) = executeLevel ((name, aux env value):env) rest
        executeLevel env (ast:[]) = aux env ast
        executeLevel env (ast:rest) = executeLevel env rest
        executeLevel _ x = AList []
        asts = (parseMany.tokenize) code
        aux env (Atom name) = case findInEnv name env of
          Just a -> a
          Nothing -> Atom name
        aux _ (AList []) = AList []
        aux env (AList (Atom "quote":n:_)) = n
        aux env (AList (Atom "'":n:_)) = n
        aux env (AList (Atom "eq?":a:b:_)) = if aux env a == aux env b then AList [AList []] else AList []
        aux env (AList (Atom "atom?":a:_)) = case (aux env a) of
          AList _ -> AList []
          _ -> aux env a
        aux env (AList (Atom "cons":x:xs:_)) = case aux env xs of
          AList xs' -> AList ((aux env x):xs')
          _ -> AList []
        aux env (AList (Atom "tail":xs:_)) = case aux env xs of
          AList xs' -> AList $ (map (aux env) (drop 1 xs'))
          _ -> AList []
        aux env (AList (Atom "first":xs:_)) = case aux env xs of
          AList (x:_) -> aux env x
          _ -> AList []
        aux env (AList (Atom "cond":vs)) = cond env vs
          where cond env (p:e:rest)
                  | aux env p == AList [] = cond env rest
                  | otherwise = aux env e
                cond _ (_:_) = error "cond must be called with test/exp pairs"
                cond _ _ = AList []
        aux env (AList (Atom "lambda":AList params:body:_)) = Fn (map (\(Atom x) -> x) params) body
        aux env (AList (fn:rest)) = apply (aux env fn) rest env
          where apply (Fn names body) values env = let nextEnv = bind names (map (aux env) values) env in aux nextEnv body
                apply (Atom fn) values env = case findMacro fn of
                  Just macro -> aux env $ macro (aux env) values
                  _ -> error $ "Could not apply fn: " ++ fn

findMacro n = aux n macros
  where aux _ [] = Nothing
        aux n ((n', m):rest) = if n == n' then Just m else aux n rest

macros = structuralMacros ++ mathyMacros

structuralMacros = [(
  "if", \_ (p:a:b:_) -> AList [Atom "cond", p, a, Atom "1", b])]

mathyMacros = [(
  "+",   \eval       -> Atom . show . foldl (\o -> (+) o . asInt . eval) 0),(
  "neg", \eval (x:_) -> Atom . show $ -(asInt $ eval x))]

asInt (Atom v) = read v

bind [] [] env = env
bind (name:names) (value:values) env = bind names values ((name, value):env)

findInEnv _ [] = Nothing
findInEnv someName ((name, value):envs)
  | name == someName = Just value
  | otherwise = findInEnv someName envs

parseMany ((RawText named):rest) = Atom named:parseMany rest
parseMany (BeginList:toParse) = AList body:parseMany rest
  where (body, rest) = untilEndList toParse []
parseMany [] = []

parse (RawText named:_) = Atom named
parse (BeginList:toParse) = AList body
  where (body, rest) = untilEndList toParse []
parse exp = error $ "Unable to parse: " ++ show exp

untilEndList (EndList:rest) acc = (reverse acc, rest)
untilEndList (BeginList:toParse) acc = untilEndList rest (AList body:acc)
  where (body, rest) = untilEndList toParse []
 
untilEndList r@(_:xs) acc = untilEndList xs (parse r:acc)
untilEndList [] _ = error "Unmatched ("

tokenize [] = []
tokenize ('(':ss) = BeginList:tokenize ss
tokenize (')':ss) = EndList:tokenize ss
tokenize (' ':ss) = tokenize ss
tokenize text = RawText rawText:tokenize remainder
  where rawTextAndRemainder r@(t:ts) acc = 
          if t `elem` " )("
            then (reverse acc, r)
            else rawTextAndRemainder ts (t:acc)
        rawTextAndRemainder [] acc = (reverse acc, [])
        (rawText, remainder) = rawTextAndRemainder text []
