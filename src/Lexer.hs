module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser def
  where def = emptyDef {
                Token.commentLine = ";" ,
                Token.reservedNames = [] ,
                Token.reservedOpNames = ["+", "-"]
                }

-- emptyDef is embedded minimal LanguageDefinition


-- embedded Parser by Parsec.Token

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

natural :: Parser Integer
natural = Token.natural lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

-- parse non-reserved name
identifier :: Parser String
identifier = Token.identifier lexer

-- parse any whitspace
whitespace :: Parser ()
whitespace = Token.whiteSpace lexer

