module Translate where

import Syntax
import Parser

--transCloToString :: Clo -> String
--transCloToString

writeTransedFile :: FilePath -> String -> IO String
writeTransedFile path target = do
  appendFile path (target ++ "\n")
  return "Success!!"
