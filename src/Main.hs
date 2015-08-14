{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Interface

data Option = Option
    { source :: String
    , output :: String
    , xos :: String
    , xbit :: String
    } deriving (Show, Data, Typeable)

option :: Option
option = Option
    { source = ""
      &= help "source file name"
    , output = ""
      &= help "executable file name"
    , xos = ""
      &= help "xcompile target os"
    , xbit = ""
      &= help "xcompile target bit"
    }
    &= summary "Clover version 1.0.0"
    &= program "clover"

main :: IO ()
main = do
  opt <- cmdArgs option
  let s = source opt
  let o = output opt
  let xo = xos opt
  let xb = xbit opt
  if (xo == "" && xb == "")
    then
      if s == ""
        then putStrLn "source file is not given: use -s option"
        else compile s o >>= putStr
    else
      if (xo == "" || xb == "")
        then putStrLn "xcompile option is not correct: use --xos & --xbit"
        else putStrLn "call compilex"
