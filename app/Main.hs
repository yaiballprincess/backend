{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)

import MyLib qualified (runApp)
import YIBP.CLI.CreateAdmin (createAdmin)
import YIBP.CLI.Migrate (doMigrations)

main :: IO ()
main = do
  getArgs >>= \case
    ("migrate" : _) -> doMigrations
    ("create-admin" : _) -> createAdmin
    _ -> MyLib.runApp
