{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module YIBP.Db.Db where

import GHC.Stack (HasCallStack, prettyCallStack, callStack)

import Control.Exception

import Hasql.Session qualified as Session
import Hasql.Connection (Connection)

newtype DbException = DbException ((HasCallStack) => Session.QueryError)

instance Show DbException where
  show (DbException qe) = "DbException " <> show qe <> "\n" <> prettyCallStack callStack

deriving anyclass instance Exception DbException

type WithDb = (?dbConn :: Connection)

withConnEither
  :: WithDb
  => (Connection -> IO a)
  -> IO a
withConnEither f = f ?dbConn

withConn
  :: WithDb
  => (Connection -> IO (Either Session.QueryError a))
  -> IO a
withConn f = withConnEither f >>= \case
  Left err -> throwIO (DbException err)
  Right x -> pure x
