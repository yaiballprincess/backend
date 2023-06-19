module MyLib (runApp) where

import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server.Generic

-- import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Reader (runReaderT)

import Hasql.Connection qualified as Connection

import Optics

import Servant.Auth.Server (defaultCookieSettings, defaultJWTSettings)
import YIBP.App
import YIBP.Config (parseConfig)
import YIBP.Server
import Servant.Auth.Server.Internal.AddSetCookie
import GHC.Generics

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
  let env = Env {dbConnection = conn, jwk = config ^. #jwk, appConfig = config}
  run 8080 $
    genericServeTWithContext
      (nt env)
      theAPI
      (defaultJWTSettings (config ^. #jwk) :. defaultCookieSettings :. EmptyContext)