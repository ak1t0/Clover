module Syntax where

data Clo = Bool Bool
         | Int Integer
         | Float Double
         | Symbol String
         | Keyword String
         | String String
         | List [Clo]
         | Vector [Clo]
         deriving (Eq, Ord)

showClo :: Clo -> String
showClo (Int i) = show i
showClo (Float f) = show f
showClo (Symbol s) = s
showClo (Keyword s) = s
showClo (String s) = show s
showClo (List l) = "(" ++ unwords (map showClo l) ++ ")"
showClo (Vector v) = "[" ++ unwords (map showClo v) ++ "]"

instance Show Clo where show = showClo
