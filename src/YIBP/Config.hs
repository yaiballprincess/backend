module YIBP.Config (Config, DbConfig, parseConfig) where

import Data.Time.Clock

import Crypto.JOSE.JWK (JWK)
import Data.Aeson qualified as J
import Data.Text qualified as T

import Control.Monad.IO.Class
import System.Environment

import Optics.TH (makeFieldLabelsNoPrefix)

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

parseDbConfig :: (MonadIO m) => m DbConfig
parseDbConfig = do
  host <- liftIO $ getEnv "DB_CONFIG_HOST"
  port :: Int <- liftIO $ read <$> getEnv "DB_CONFIG_PORT"
  user <- liftIO $ getEnv "DB_CONFIG_USER"
  password <- liftIO $ getEnv "DB_CONFIG_PASSWORD"
  dbName <- liftIO $ getEnv "DB_CONFIG_DB"
  pure $
    DbConfig
      { host = T.pack host
      , port = port
      , user = T.pack user
      , password = T.pack password
      , db = T.pack dbName
      }

parseConfig :: (MonadIO m, MonadFail m) => m Config
parseConfig = do
  dbConfig <- parseDbConfig
  jwtFilePath <- liftIO $ getEnv "JWT_CONFIG_FILEPATH"
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