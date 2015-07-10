module Translate where

import Syntax
import Parser

translate :: String -> IO String
translate input =
  writeTransedFile "t.go" $ transClo $ parsePrim input

translatei :: String -> IO String
translatei input =
  writeTransedFilei "repl.go" $ transClo $ parsePrim "(defn f-name [x y] (plus x y))"

-- AST translating function
transClo :: Either a Clo -> String
transClo r = case r of
  Right a -> transCloToString a
  Left b -> "Clo source is not correct!!"

transCloToString :: Clo -> String
transCloToString o = case o of
  List [Symbol "defn", Symbol "main", args, body]
    -> generateFuncMain body
  List [Symbol "defn", fname, args, body]
    -> generateFunc fname args body
  --  -> "defn" ++ " " ++ (show fname) ++ " " ++ (show args) ++ " "++ (show body)
  _ -> "not math!"

-- File I/O function
writeTransedFile :: FilePath -> String -> IO String
writeTransedFile path target = do
  base <- readFile "res/base.go"
  writeFile path (base ++ target ++ "\n" ++ "\n")
  return "Success!!"

writeTransedFilei :: FilePath -> String -> IO String
writeTransedFilei path target = do
  appendFile path (target ++ "\n")
  return "Success!!"

-- const
baseData :: String
baseData = "func generated(x, y CloverObj) CloverObj {\n\treturn Plus(x, y)\n}\n"


-- code generation
generateFunc :: Clo -> Clo -> Clo -> String
generateFunc fname args body =
  "func " ++ (takeSymbol fname) ++
  (generateFuncArgs args) ++ " CloverObj " ++
  (generateFuncBody body)

generateFuncMain :: Clo -> String
generateFuncMain body =
  "func " ++ "main" ++ "()" ++ " " ++ (generateFuncMainBody body)

generateFuncArgs :: Clo -> String
generateFuncArgs args = parenter $ generateFuncArgsGen $ takeVector args

generateFuncArgsGen :: [Clo] -> String
generateFuncArgsGen [] = " CloverObj"
generateFuncArgsGen [x] = (takeSymbol x) ++ (generateFuncArgsGen [])
generateFuncArgsGen (x:xs) = (takeSymbol x) ++ ", " ++ (generateFuncArgsGen xs)

-- List[sy1 sy2 sy3]
-- sy1(sy2, sy3)

generateFuncBody :: Clo -> String
generateFuncBody body =
  "{\n\t" ++
  "v := " ++ (generateFuncBodyArgs body) ++ "\n" ++
  "\t" ++ "return v" ++ "\n" ++
  "}"

generateFuncMainBody :: Clo -> String
generateFuncMainBody body =
  "{\n\t" ++ (generateFuncBodyArgs body) ++ "\n" ++ "}"

-- AST in function args to String
--
generateFuncBodyArgs :: Clo -> String
generateFuncBodyArgs (Symbol x) = x
generateFuncBodyArgs (Int x) = "CloverInt{" ++ (show x) ++ "}"
generateFuncBodyArgs (Float x) = "CloverFloat{" ++ (show x) ++ "}"
generateFuncBodyArgs (List (x:xs)) =
  (generateFuncBodyArgs x) ++
  (parenter $ init $ unwords $ map (\x -> (generateFuncBodyArgs x) ++ ",") xs)

parenter :: String -> String
parenter s = "(" ++ s ++ ")"

takeVector :: Clo -> [Clo]
takeVector (Vector x) = x

takeList :: Clo -> [Clo]
takeList (List x) = x

takeSymbol :: Clo -> String
takeSymbol (Symbol x) = x

strClo :: Clo -> String
strClo (Int i) = show i
strClo (Float f) = show f
strClo (Symbol s) = s
strClo (Keyword s) = ":" ++ s
strClo (String s) = show s
strClo (List l) = "(" ++ unwords (map strClo l) ++ ")"
strClo (Vector v) = "[" ++ unwords (map strClo v) ++ "]"
