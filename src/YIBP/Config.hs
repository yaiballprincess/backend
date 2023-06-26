module YIBP.Config where

import Data.Time.Clock

import Crypto.JOSE.JWK (JWK)
import Data.Aeson qualified as J
import Data.Text qualified as T

import Control.Monad.IO.Class
import System.Environment

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
  }
  deriving (Show, Eq)

makeFieldLabelsNoPrefix ''DbConfig
makeFieldLabelsNoPrefix ''Config

type WithConfig = (?appConfig :: Config)

withConfig :: (WithConfig) => (Config -> a) -> a
withConfig f = f ?appConfig


getConfig :: WithConfig => Config
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

parseConfig :: IO Config
parseConfig = do
  dbConfig <- parseDbConfig
  jwtFilePath <- getEnv "JWT_CONFIG_FILEPATH"
  Just (jwkConfig :: JWK) <- liftIO $ J.decodeFileStrict' jwtFilePath
  pure $
    Config
      { dbSettings = dbConfig
      , jwk = jwkConfig
      , accessTokenDuration = 30 * minuteInSeconds
      , refreshTokenDuration = 60 * nominalDay
      }
  where
    minuteInSeconds = 60