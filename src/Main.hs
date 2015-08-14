{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Interface

data Option = Option
    { source :: String
    , output :: String
    } deriving (Show, Data, Typeable)

option :: Option
option = Option
    { source = ""
      &= help "source file name"
    , output = ""
      &= help "executable file name"
    }
    &= summary "Clover version 1.0.0"
    &= program "clover"

main :: IO ()
main = do
  opt <- cmdArgs option
  let s = source opt
  let o = output opt
  if s == ""
    then putStrLn "source file is not given: use -s option"
    else compile s o >>= putStr
