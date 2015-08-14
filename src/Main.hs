{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs

import Interface

data Option = Option
    { source :: String
    , output :: String
    , xos :: String
    , xarch :: String
    } deriving (Show, Data, Typeable)

option :: Option
option = Option
    { source = ""
      &= help "source file name"
    , output = ""
      &= help "executable file name"
    , xos = ""
      &= help "xcompile target os"
    , xarch = ""
      &= help "xcompile target arch"
    }
    &= summary "Clover version 1.0.0"
    &= program "clover"

main :: IO ()
main = do
  opt <- cmdArgs option
  let s = source opt
  let o = output opt
  let xo = xos opt
  let xa = xarch opt
  if (xo == "" && xa == "")
    then
      if s == ""
        then putStrLn "source file is not given: use -s option"
        else compile s o >>= putStr
    else
      if (xo == "" || xa == "")
        then putStrLn "xcompile option is not correct: use --xos & --xarch"
        else xcompile s o [xo, xa] >>= putStr
