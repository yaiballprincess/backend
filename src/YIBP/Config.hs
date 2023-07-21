module YIBP.Config where

import Data.Time.Clock

import Crypto.JOSE.JWK (JWK)
import Data.Aeson qualified as J
import Data.Text qualified as T

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import System.OsPath

import Optics (makeFieldLabelsNoPrefix)

data DbConfig = DbConfig
  { host :: !T.Text
  , port :: !Int
  , user :: !T.Text
  , password :: !T.Text
  , db :: !T.Text
  }
  deriving (Show, Eq)

data Config = Config
  { dbSettings :: !DbConfig
  , jwk :: !JWK
  , accessTokenDuration :: !NominalDiffTime
  , refreshTokenDuration :: !NominalDiffTime
  , logDirectory :: !OsPath
  , serverPort :: !Int
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''DbConfig
makeFieldLabelsNoPrefix ''Config

type WithConfig = (?appConfig :: Config)

withConfig :: (WithConfig) => (Config -> a) -> a
withConfig f = f ?appConfig

getConfig :: (WithConfig) => Config
getConfig = ?appConfig

parseDbConfig :: IO DbConfig
parseDbConfig = do
  host <- getEnv "DB_CONFIG_HOST"
  port :: Int <- read <$> getEnv "DB_CONFIG_PORT"
  user <- getEnv "DB_CONFIG_USER"
  password <- getEnv "DB_CONFIG_PASSWORD"
  dbName <- getEnv "DB_CONFIG_DB"
  pure $
    DbConfig
      { host = T.pack host
      , port = port
      , user = T.pack user
      , password = T.pack password
      , db = T.pack dbName
      }

data InvalidLogDirectory = InvalidLogDirectory
  deriving (Show, Eq, Exception)

parseConfig :: IO Config
parseConfig = do
  dbConfig <- parseDbConfig
  jwtFilePath <- getEnv "JWT_CONFIG_FILEPATH"
  logDirectory <- encodeFS =<< getEnv "LOG_DIRECTORY"
  unless (isValid logDirectory) $ do
    throwIO InvalidLogDirectory
  Just (jwkConfig :: JWK) <- liftIO $ J.decodeFileStrict' jwtFilePath
  port <- read <$> getEnv "YIBP_BACKEND_PORT"
  pure $
    Config
      { dbSettings = dbConfig
      , jwk = jwkConfig
      , accessTokenDuration = 30 * minuteInSeconds
      , refreshTokenDuration = 60 * nominalDay
      , logDirectory = logDirectory
      , serverPort = port
      }
  where
    minuteInSeconds = 60
