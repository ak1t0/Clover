import System.Console.Haskeline

import Syntax
import Lexer
import Parser

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      maybeinput <- getInputLine "> "
      case maybeinput of
        Nothing -> outputStrLn "Goodbye."
        Just "quit" -> outputStrLn "Goodbye."
        Just input -> (outputStrLn $ "Input was: " ++ (parseClover input)) >> loop
