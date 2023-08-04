{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)

import MyLib qualified (runApp)
import YIBP.CLI.CreateAdmin (createAdmin)
import YIBP.CLI.Migrate (doMigrations)

helpMessage :: String
helpMessage = "Usage: ./program-name [ACTION]\n\
              \  [ACTION] \n\
              \    help - show help \n\
              \    migrate - run migrations \n\
              \    create-admin - create user 'admin' \n\
              \    <otherwise> - run application"

main :: IO ()
main = do
  getArgs >>= \case
    ("help" : _) -> putStrLn helpMessage
    ("migrate" : _) -> doMigrations
    ("create-admin" : _) -> createAdmin
    _ -> MyLib.runApp
