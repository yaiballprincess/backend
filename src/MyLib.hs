{-# LANGUAGE OverloadedStrings #-}
module MyLib (runApp) where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server.Generic

-- import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Optics

import Hasql.Connection
import Hasql.Connection qualified as Connection
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings)
import YIBP.Config
import YIBP.Db
import YIBP.Logger
import YIBP.Scheduler
import YIBP.Scheduler.Scheduler
import YIBP.Server

runWithScheduler :: Scheduler -> ((WithScheduler) => IO a) -> IO a
runWithScheduler sc a = let ?scheduler = sc in a

runWithDb :: Connection -> ((WithDb) => IO a) -> IO a
runWithDb conn a = let ?dbConn = conn in a

runWithConfig :: Config -> ((WithConfig) => IO a) -> IO a
runWithConfig config a = let ?appConfig = config in a

getConnectionSettings :: DbConfig -> Connection.Settings
getConnectionSettings dbConf =
  Connection.settings
    (encodeUtf8 dbConf.host)
    (fromIntegral dbConf.port)
    (encodeUtf8 dbConf.user)
    (encodeUtf8 dbConf.password)
    (encodeUtf8 dbConf.db)

runApp :: IO ()
runApp = do
  config <- parseConfig
  let connSettings = getConnectionSettings config.dbSettings
  Right conn <- Connection.acquire connSettings
  scheduler <- mkScheduler
  runWithLogger config $
    runWithConfig config $
      runWithDb conn $
        runWithScheduler scheduler $ do
          _ <- withLabel "service" ("scheduler" :: String) $ loggedForkIO runScheduler
          _ <- loggedForkIO initScheduler
          run config.serverPort $
            genericServeTWithContext
              id
              theAPI
              (defaultJWTSettings (config ^. #jwk) :. defaultCookieSettings :. EmptyContext)
