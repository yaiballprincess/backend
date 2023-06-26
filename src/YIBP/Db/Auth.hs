module YIBP.Db.Auth (insertRefreshToken, getCreatedAtByRefreshToken, updateRefreshToken) where

import Data.Int
import Data.Time qualified as Time
import Data.UUID (UUID)
import Data.Bifunctor

import GHC.Stack (HasCallStack)

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH

import YIBP.Db.Db

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
  :: (WithDb, HasCallStack)
  => UUID
  -> Int
  -> IO (Maybe Time.UTCTime)
insertRefreshToken uuid ownerId = do
  withConn $ Session.run (insertRefreshTokenSession uuid ownerId)

getCreatedAtByRefreshTokenSession :: UUID -> Session.Session (Maybe Time.UTCTime)
getCreatedAtByRefreshTokenSession uuid = Session.statement uuid stmt
  where
    stmt :: Statement UUID (Maybe Time.UTCTime)
    stmt =
      [TH.maybeStatement| 
      select ("created_at" :: TIMESTAMPTZ) 
      from "session" where "id" = ($1 :: uuid)
    |]

getCreatedAtByRefreshToken
  :: (WithDb, HasCallStack)
  => UUID
  -> IO (Maybe Time.UTCTime)
getCreatedAtByRefreshToken uuid = do
  withConn $ Session.run (getCreatedAtByRefreshTokenSession uuid)

updateRefreshTokenSession :: UUID -> UUID -> Session.Session (Maybe (Time.UTCTime, Int))
updateRefreshTokenSession old new = fmap (second fromIntegral) <$> Session.statement (old, new) stmt
  where
    stmt :: Statement (UUID, UUID) (Maybe (Time.UTCTime, Int32))
    stmt =
      [TH.maybeStatement|
      update "session"
      set 
        "id" = ($2 :: uuid),
        "created_at" = now()
      where "id" = ($1 :: uuid)
      returning ("created_at" :: TIMESTAMPTZ), ("owner_id" :: int4)
    |]

updateRefreshToken
  :: (WithDb, HasCallStack)
  => UUID
  -> UUID
  -> IO (Maybe (Time.UTCTime, Int))
updateRefreshToken old new = do
  withConn $ Session.run (updateRefreshTokenSession old new)