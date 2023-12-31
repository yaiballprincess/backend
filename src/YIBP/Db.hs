{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module YIBP.Db where

import GHC.Stack (HasCallStack, callStack, prettyCallStack)

import Control.Exception

import Data.Text.Encoding (encodeUtf8)

import Hasql.Connection (Connection)
import Hasql.Connection qualified as Connection
import Hasql.Session qualified as Session

import YIBP.Config

newtype DbException = DbException ((HasCallStack) => Session.QueryError)

instance Show DbException where
  show (DbException qe) = "DbException " <> show qe <> "\n" <> prettyCallStack callStack

deriving anyclass instance Exception DbException

type WithDb = (?dbConn :: Connection)

runWithDb :: Connection -> ((WithDb) => IO a) -> IO a
runWithDb conn a = let ?dbConn = conn in a

withConnEither
  :: (WithDb)
  => (Connection -> IO a)
  -> IO a
withConnEither f = f ?dbConn

withConn
  :: (WithDb)
  => (Connection -> IO (Either Session.QueryError a))
  -> IO a
withConn f =
  withConnEither f >>= \case
    Left err -> throwIO (DbException err)
    Right x -> pure x

getConnectionSettings :: DbConfig -> Connection.Settings
getConnectionSettings dbConf =
  Connection.settings
    (encodeUtf8 dbConf.host)
    (fromIntegral dbConf.port)
    (encodeUtf8 dbConf.user)
    (encodeUtf8 dbConf.password)
    (encodeUtf8 dbConf.db)

makeConnection :: DbConfig -> IO Connection
makeConnection cfg =
  Connection.acquire connSettings >>= \case
    Left err -> error $ "an error occured while connecting to database: " <> show err
    Right x -> pure x
  where
    connSettings = getConnectionSettings cfg
