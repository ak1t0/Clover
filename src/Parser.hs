module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Token
-- import qualified Text.Parsec as

import Lexer
import Syntax

---- Parser ----


--- High-level Parser

-- top-level parser for Main
parseClover :: String -> String
parseClover target = (parseCore parseExpr "target" target) ++ " parsed!!!!!"

-- Parser test function for cabal repl
parseCore :: (Show a) => Parser a -> SourceName -> String -> String
parseCore p s i = case (parse p s i) of
  Right a -> show a
  Left e -> show "Error"

-- for Translate
parsePrim :: String -> Either ParseError Clo
parsePrim i = parse parseExpr "target" i

-- combinated parser
parseExpr :: Parser Clo
parseExpr = try parseSymbolicOp
        <|> try parseFloat
        <|> try parseInt
        <|> try parseKeyword
        <|> try parseBool
        <|> try parseSymbol
        <|> try parseString
        <|> parseList
        <|> parseVector


--- Primitive Parser

-- Clover Symbol
-- a-z, A-Z, 0-9
parseSymbol :: Parser Clo
parseSymbol = do
  x <- letter <|> symbolic
  xs <- many (letter <|> digit <|> symbolic)
  return $ Symbol $ x:xs

-- Clover Keyword
-- : a-z, A-Z, 0-9
parseKeyword :: Parser Clo
parseKeyword = do
  char ':'
  x <- letter <|> symbolic
  xs <- many (letter <|> digit <|> symbolic)
  return $ Keyword (x:xs)

-- Clover Bool
-- true, false
parseBool :: Parser Clo
parseBool = do
  x <- string "true" <|> string "false"
  return $ case x of
    "true" -> Bool True
    "false" -> Bool False

-- Clover String
-- " String "
parseString :: Parser Clo
parseString = do
  char '"'
  x <- many1 $ (escapedChar <|> noneOf ['"'])
  char '"'
  return $ String x

-- Clover Integer
-- +/- n, n
parseInt :: Parser Clo
parseInt = do
  x <- try integer <|> natural
  return $ Int x

-- Clover Float
-- n.m, non-signed, signed
parseFloat :: Parser Clo
parseFloat = do
  s <- many $ char '-'
  x <- float
  return $ case s of
    [] -> Float x
    _ -> Float (-x)

-- Clover Symbolic
-- #, !, $, &, |, /, ?, @, ^, _, ~, ', -, =
symbolic :: Parser Char
symbolic = oneOf "#!$&|/?@^_~'-=<>"

-- Clover Symbolic Op
-- +. *, -
parseSymbolicOp :: Parser Clo
parseSymbolicOp = do
  x <- oneOf "+-*"
  xs <- noneOf "0123456789"
  return $ case x of
    '+' -> Symbol "+"
    '-' -> Symbol "-"
    '*' -> Symbol "*"

-- Clover Escaped Character
-- \n, \\, \"
escapedChar :: Parser Char
escapedChar = do
  char '\\'
  x <- oneOf "\"\\n"
  return $ case x of
    'n' -> '\n'
    _ -> x

-- Clover List
-- (_ _ _ _ ...)
parseList :: Parser Clo
parseList = do
  char '('
  x <- sepBy parseExpr spaces
  char ')'
  return $ List x

-- Clover Vector
-- [_ _ _ ...]
parseVector :: Parser Clo
parseVector = do
  char '['
  x <- sepBy parseExpr spaces
  char ']'
  return $ Vector x
