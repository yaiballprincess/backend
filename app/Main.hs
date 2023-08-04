{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)

import MyLib qualified (runApp)
import YIBP.CLI.Migrate (doMigrations)

main :: IO ()
main = do
  getArgs >>= \case
    ("migrate" : _) -> doMigrations
    _ -> MyLib.runApp
