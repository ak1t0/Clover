module Interface where

import Data.List

splitSource :: String -> String -> [Int] -> [String]
-- check all string
splitSource "" res p = if (head p) == (last p)
  then [res ++ (show p)]
  else ["Error"]
splitSource t res p = if ((head p) == (last p) && (head p) /= 0)
  then [res ++ (show p), t]
  else splitSource (tail t) (res ++ [ch]) (check ch p)
  where
    ch = head t
-- check
check :: Char -> [Int] -> [Int]
check ch [l, r] = case ch of
  '(' -> [l+1, r]
  ')' -> [l, r+1]
  _ -> [l, r]

search :: Char -> String -> Maybe Int
search ch str = elemIndex ch str

checkParentLR :: String -> Bool
checkParentLR str = search '(' str < search ')' str

cleanSource :: String -> String
cleanSource str =
  map (\x -> if ((x == '\n') || (x == '\t')) then ' '; else x) str
