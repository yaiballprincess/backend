{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module YIBP.Db.Util (WithDb, withConn, withConn', liftError) where

import Control.Exception
import Control.Monad.Reader
import GHC.Stack (HasCallStack, prettyCallStack, callStack)
import Hasql.Connection (Connection)
import Hasql.Session qualified as Session
import YIBP.App (Has (obtain))


newtype DbException = DbException (HasCallStack => Session.QueryError)

instance Show DbException where
  show (DbException qe) = "DbException " <> show qe <> "\n" <> prettyCallStack callStack
deriving anyclass instance Exception DbException

type WithDb env m = (MonadReader env m, Has Connection env, MonadIO m)

withConn
  :: WithDb env m
  => (Connection -> IO a)
  -> m a
withConn f = do
  conn <- asks $ obtain @Connection
  liftIO $ f conn

liftError :: (HasCallStack, MonadIO m) => Either Session.QueryError a -> m a
liftError = either (liftIO . throwIO) pure

withConn'
  :: WithDb env m
  => (Connection -> IO (Either Session.QueryError a))
  -> m a
withConn' f = withConn f >>= liftError 