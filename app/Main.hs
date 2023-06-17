{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import qualified MyLib (runApp)
import GHC.Stack
import Control.Exception


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.runApp
