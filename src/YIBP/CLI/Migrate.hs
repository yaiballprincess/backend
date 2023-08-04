{-# LANGUAGE OverloadedStrings #-}

module YIBP.CLI.Migrate where

import Data.FileEmbed

import YIBP.Config
import YIBP.Db

import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session

import Data.Maybe (fromMaybe)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T

import Fmt

doMigrations :: IO ()
doMigrations = do
  cfg <- parseConfig
  let connSettings = getConnectionSettings cfg.dbSettings
  Right conn <- Connection.acquire connSettings
  Session.run (Session.sql migrations) conn >>= \case
    Right () -> putStrLn "succesfully applied migrations"
    Left (Session.QueryError _ _ (Session.ClientError t)) -> do
      let errMsg = T.decodeUtf8 $ fromMaybe "" t
      T.putStrLn $ "Client error: " +| errMsg |+ ""
    Left (Session.QueryError _ _ (Session.ResultError err)) -> do
      T.putStrLn $ "Result error: " +| show err |+ ""
  where
    migrations = $(embedFile "extra/migrations/001.psql")
