module YIBP.Db.Auth (insertRefreshToken) where

import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.UUID (UUID)

import GHC.Stack (HasCallStack)

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH

import YIBP.Db.Util

insertRefreshTokenSession :: UUID -> Int -> Session.Session (Maybe Time.UTCTime)
insertRefreshTokenSession uuid ownerId = Session.statement (uuid, fromIntegral ownerId) stmt
  where
    stmt :: Statement (UUID, Int32) (Maybe Time.UTCTime)
    stmt =
      [TH.maybeStatement|
      insert into "session" ("id", "owner_id")
      values ($1 :: uuid, $2 :: int4)
      on conflict do nothing
      returning ("created_at" :: TIMESTAMPTZ)
    |]

insertRefreshToken
  :: (WithDb env m, HasCallStack)
  => UUID
  -> Int
  -> m (Maybe Time.UTCTime)
insertRefreshToken uuid ownerId = do
  r <- withConn $ Session.run (insertRefreshTokenSession uuid ownerId)
  liftError r