module Translate where

import Syntax
import Parser

import Data.Char
import System.Process

-- function for debug

-- translate and overwrite
translate :: String -> IO String
translate input = writeTransedFile "t.go" $ transClo $ parsePrim input

-- translate and append
transadd :: String -> IO String
transadd input = writeTransedFilep "t.go" $ transClo $ parsePrim input

-- for confirm gorun build and run result
gorun :: IO String
gorun = readProcess "go" ["run", "t.go"] []

-- use from compile
gobuild :: String -> String -> IO String
gobuild file source = do
  (_, _, e) <- readProcessWithExitCode "go" ["build", "-o", file, source] []
  return e

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
  return path

writeTransedFilep :: FilePath -> String -> IO String
writeTransedFilep path target = do
  appendFile path (target ++ "\n")
  return "Success!!"

-- code generation
generateFunc :: Clo -> Clo -> Clo -> String
generateFunc fname args body =
  "var " ++ name ++ " CloverObj = CloverFunc{" ++ name ++ "Func}\n"++
  "func " ++ (name ++ "Func") ++ "(o ...interface{}) CloverObj " ++
  (generateFuncBody args body)
  where
    name = takeSymbol fname

generateFuncMain :: Clo -> String
generateFuncMain body =
  "func " ++ "main" ++ "()" ++ " " ++ (generateFuncMainBody body)

generateFuncArgs :: Clo -> String
generateFuncArgs args =
  foldl (++) "objs := preprocess(o)\n\t" $
  map (++ "]\n\t") $ addIndexToEnd $ map (++ " := objs[") $
  map takeSymbol $ takeVector args

generateFuncBody :: Clo -> Clo -> String
generateFuncBody args body =
  "{\n\t" ++
  generateFuncArgs args ++
  "v := " ++ (astBodyToString body) ++ "\n" ++
  "\t" ++ "return v" ++ "\n" ++
  "}"

generateFuncMainBody :: Clo -> String
generateFuncMainBody body =
  "{\n\t" ++ (astBodyToString body) ++ "\n" ++ "}"

-- AST in function args to String
--
astBodyToString :: Clo -> String
astBodyToString (Symbol "nil") = "CloverNil{0}"
astBodyToString (Bool x) = "CloverBool{" ++ (map toLower (show x)) ++ "}"
astBodyToString (Int x) = "CloverInt{" ++ (show x) ++ "}"
astBodyToString (Float x) = "CloverFloat{" ++ (show x) ++ "}"
astBodyToString (String x) = "CloverString{" ++ (show x) ++ "}"
astBodyToString (Symbol x) = symbolToFunc x
astBodyToString (Vector x) =
  "CloverVector{[]CloverObj{" ++ (commaer $ map astBodyToString x) ++ "}}"
-- spcial form
astBodyToString (List ((Symbol "if"):xs)) =
  "If" ++ (parenter $ commaer [b, t, f])
  where
    xs' = map astBodyToString xs
    b = head xs'
    t = wrapByFunc $ head $ tail xs'
    f = wrapByFunc $ last xs'
astBodyToString (List ((Symbol "fn"):xs)) =
  "objnize(CloverFunc{" ++ (addFnHead $ generateFuncBody args body) ++  "})"
  where
    args = head xs
    body = head $ tail xs
-- List to func(args...)
astBodyToString (List (x:xs)) =
  (astBodyToString x) ++ ".(CloverFunc).value" ++
  (parenter $ commaer $ map astBodyToString xs)

-- for built-in function
symbolToFunc :: String -> String
symbolToFunc s = case s of
    "+" -> "Plus"
    "-" -> "Minus"
    "*" -> "Mul"
    "==" -> "Eq"
    "not" -> "Not"
    "and" -> "And"
    "or" -> "Or"
    ">" -> "Gr"
    "<" -> "Le"
    ">=" -> "Gre"
    "<=" -> "Lee"
    x -> x

parenter :: String -> String
parenter s = "(" ++ s ++ ")"

commaer :: [String] -> String
commaer s = init $ unwords $ map (++ ",") s

addIndexToEnd :: [String] -> [String]
addIndexToEnd strs = interjoin strs (map show [0..(length(strs) - 1)])

interjoin :: [String] -> [String] -> [String]
interjoin [] [] = []
interjoin (x:xs) (y:ys) = [x ++ y] ++ (interjoin xs ys)

wrapByFunc :: String -> String
wrapByFunc x = "func(...interface{}) CloverObj {return " ++ x ++ "}"

addFnHead :: String -> String
addFnHead x = "func(o ...interface{}) CloverObj " ++ x

takeVector :: Clo -> [Clo]
takeVector (Vector x) = x

takeList :: Clo -> [Clo]
takeList (List x) = x

takeSymbol :: Clo -> String
takeSymbol (Symbol x) = x
