import System.Console.Haskeline
import Control.Monad.IO.Class

import Syntax
import Lexer
import Parser
import Translate

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      maybeinput <- getInputLine "> "
      case maybeinput of
        Nothing -> outputStrLn "Goodbye."
        Just "quit" -> outputStrLn "Goodbye."
        Just input -> do
          x <- liftIO $ translatei input
          outputStrLn x
          loop
          --return expression
        -- (outputStrLn $ "Input was: " ++ (parseClover input)) >> loop
