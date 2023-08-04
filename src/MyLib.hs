{-# LANGUAGE OverloadedStrings #-}

module MyLib (runApp) where

import Network.Wai.Handler.Warp
  ( defaultOnException
  , defaultSettings
  , runSettings
  , setOnException
  , setPort
  )
import Servant
import Servant.Server.Generic

import Data.Function ((&))

import Hasql.Connection
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings)
import YIBP.Config
import YIBP.Db
import YIBP.Logger
import YIBP.Scheduler
import YIBP.Scheduler.Scheduler
import YIBP.Server

import Fmt

runWithScheduler :: Scheduler -> ((WithScheduler) => IO a) -> IO a
runWithScheduler sc a = let ?scheduler = sc in a

runWithConfig :: Config -> ((WithConfig) => IO a) -> IO a
runWithConfig config a = let ?appConfig = config in a

runApp :: IO ()
runApp = do
  config <- parseConfig
  conn <- makeConnection config.dbSettings
  scheduler <- mkScheduler
  runWithLogger config $
    runWithConfig config $
      runWithDb conn $
        runWithScheduler scheduler $ do
          _ <- withLabel "service" ("scheduler" :: String) $ loggedForkIO runScheduler
          _ <- loggedForkIO initScheduler
          runSettings (serverSettings config) $
            loggerMiddleware $
              genericServeTWithContext
                id
                theAPI
                (defaultJWTSettings config.jwk :. defaultCookieSettings :. EmptyContext)
  where
    serverSettings cfg = defaultSettings & setPort cfg.serverPort & setOnException onExceptionHandler
    onExceptionHandler req exc = do
      withLabel "service" ("server" :: String) $
        withLabel "exception" (show exc) $ do
          logMsg Error $ "Exception " +| show exc |+ " while handling request " +| show req |+ ""
      defaultOnException req exc
    loggerMiddleware app req respFunc = do
      withLabel "service" ("server" :: String) $ do
        logMsg Debug $ "New request: " +| show req |+ ""
        app req respFunc
