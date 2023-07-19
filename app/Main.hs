{-# LANGUAGE DerivingStrategies #-}

module Main where

import MyLib qualified (runApp)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.runApp
