{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

data Option = Option
    { source :: String
    , output :: String
    } deriving (Show, Data, Typeable)

option :: Option
option = Option
    { source = ""
      &= help "source file name"
    , output = "a.out"
      &= help "executable file name"
    }
    &= summary "Clover version 1.0.0"
    &= program "clover"


main :: IO ()
main = do
  opt <- cmdArgs option
  print opt
