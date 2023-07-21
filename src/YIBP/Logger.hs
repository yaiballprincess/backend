{-# LANGUAGE OverloadedStrings #-}

module YIBP.Logger
  ( runWithLogger
  , loggedForkIO
  , WithLogger
  , module Pew.Logger
  , module Pew.Severity
  , L.logMsg
  , L.withLabel
  , L.withLabels
  ) where

import Pew.Logger
import Pew.Logger.Config
import Pew.Logger.Implicit qualified as L
import Pew.Logger.Output.Json
import Pew.Severity

import YIBP.Config

import System.Directory.OsPath
import System.File.OsPath (withFile)
import System.IO (IOMode (..))
import System.OsPath

import Data.ByteString.Builder
import Data.Functor
import Data.Time

import Control.Concurrent

type WithLogger = (L.WithLogger_ Severity)

runWithLogger :: Config -> ((L.WithLogger_ Severity) => IO a) -> IO a
runWithLogger cfg act = do
  _ <- createDirectoryIfMissing True cfg.logDirectory
  path <-
    getCurrentTime
      <&> formatTime defaultTimeLocale "%Y-%m-%d_%H_%M_%S"
      <&> (<> ".log")
      >>= encodeFS
      <&> (\p -> cfg.logDirectory </> p)
  withFile path WriteMode $ \handle -> do
    logger <- mkJsonLogger (LoggerConfig severityToText (const True)) (hPutBuilder handle)
    L.withLogger logger act

loggedForkIO :: (WithLogger) => ((WithLogger) => IO a) -> IO ThreadId
loggedForkIO act = forkIO $ do
  threadId <- myThreadId
  void $ L.withLabel "threadId" (show threadId) act
