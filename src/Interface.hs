module Interface where

import Data.List

import Translate
import Parser

-- count parent to split source
splitSource :: String -> String -> [Int] -> [String]
splitSource "" res p = if l == r
  then [res, ""]
  else ["Error"]
  where
    l = head p
    r = last p
splitSource t res p = if (l == r && l /= 0)
  then [res, t]
  else splitSource (tail t) (res ++ [ch]) (check ch p)
  where
    ch = head t
    l = head p
    r = last p

check :: Char -> [Int] -> [Int]
check ch [l, r] = case ch of
  '(' -> [l+1, r]
  ')' -> [l, r+1]
  _ -> [l, r]

-- split source recursively
reader :: String -> [String]
reader str = if xs == ""
  then [x]
  else [x] ++ (splitSource xs "" [0,0])
  where
    res = splitSource str "" [0,0]
    x = head res
    xs = last res

-- format reading source and remove noise
passReader :: String -> [String]
passReader str = filter (elem '(') $ map format $ reader str

-- replace \n and \t to \space
cleanSource :: String -> String
cleanSource str =
  map (\x -> if ((x == '\n') || (x == '\t')) then ' '; else x) str

-- check parent direction and number
checkParent :: Int -> String -> Bool
checkParent p "" = if (p == 0)
  then True
  else False
checkParent p (x:xs) = if (p >= 0)
  then
    case x of
      '(' -> checkParent (p + 1) xs
      ')' -> checkParent (p - 1) xs
      _ -> checkParent p xs
  else False

format :: String -> String
format str = dropWhile spaceOrNewline str

spaceOrNewline :: Char -> Bool
spaceOrNewline x = if (x == '\n') || (x == ' ') then True else False

-- compile clo to executable file
compile :: String -> String -> IO String
compile target output = do
  x <- readFile target
  let y = cleanSource x
  let z = passReader y
  let c = nub $ map (checkParent 0) z
  if c == [True]
    then do
      let ir = foldr (++) "" $ map ((++ "\n\n") . transClo . parsePrim) z
      irpath <- writeTransedFile "ir.go" ir
      r <- gobuild output irpath
      if r == ""
        then return ""
        else return $ "go compile error: \n" ++ r ++ "\n"
    else
      return "parse error: check source code\n"
