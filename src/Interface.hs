module Interface where

reader :: String -> String -> [Int] -> String
-- check all string
reader "" res p = if (head p) == (last p)
  then res ++ (show p)
  else "Error"
reader t res p = if ((head p) == (last p) && (head p) /= 0)
  then res ++ (show p)
  else reader (tail t) (res ++ [ch]) (check ch p)
  where
    ch = head t
-- check
check :: Char -> [Int] -> [Int]
check ch [l, r] = case ch of
  '(' -> [l+1, r]
  ')' -> [l, r+1]
  _ -> [l, r]
