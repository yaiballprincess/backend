module MyLib (runApp) where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server.Generic

-- import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Reader (runReaderT)

import Optics

import Control.Concurrent
import Data.Vector qualified as V
import GHC.Generics
import Hasql.Connection qualified as Connection
import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings)
import Servant.Auth.Server.Internal.AddSetCookie
import YIBP.App
import YIBP.Config 
import YIBP.Db.Receiver
import YIBP.Scheduler.Scheduler
import YIBP.Server
import Hasql.Connection
import YIBP.Db.Db


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
  runWithConfig config $ runWithDb conn $ runWithScheduler scheduler $ do
    _ <- forkIO runScheduler 
    _ <- forkIO initScheduler 
    run 8080 $
      genericServeTWithContext
        id
        theAPI
        (defaultJWTSettings (config ^. #jwk) :. defaultCookieSettings :. EmptyContext)