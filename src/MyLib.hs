module MyLib (runApp) where

import Servant
import Servant.API
import Servant.Server
import Servant.Server.Generic
import Servant.API.Generic
import Network.Wai.Handler.Warp   (run)

-- import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.IO.Class

import Hasql.Connection qualified as Connection

import Optics

import YIBP.App
import YIBP.Config (parseConfig)
import YIBP.Server
import Servant.Auth.Server (defaultJWTSettings, defaultCookieSettings)


nt :: Env -> AppT Handler a -> Handler a
nt e x = runReaderT x e

runApp :: IO ()
runApp = do
  config <- parseConfig
  let dbConf = config ^. #dbSettings
  let connSettings =
        Connection.settings
          (encodeUtf8 (dbConf ^. #host))
          (fromIntegral (dbConf ^. #port))
          (encodeUtf8 (dbConf ^. #user))
          (encodeUtf8 (dbConf ^. #password))
          (encodeUtf8 (dbConf ^. #db))
  Right conn <- Connection.acquire connSettings
  let env = Env {dbConnection = conn, jwk = config ^. #jwk}
  run 8080 $ genericServeTWithContext
    (nt env)
    theAPI
    (defaultJWTSettings (config ^. #jwk) :. defaultCookieSettings :. EmptyContext)