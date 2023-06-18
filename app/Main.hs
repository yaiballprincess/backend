{-# LANGUAGE DerivingStrategies #-}
module Main where

import qualified MyLib (runApp)


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.runApp
