module YIBP.Db.User (isUserAdmin, insertUser, findUserByUsername) where

import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T

import GHC.Stack (HasCallStack)

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH

import YIBP.Db.Db
import Data.Bifunctor

isUserAdmin
  :: (WithDb, HasCallStack)
  => Int
  -> IO (Maybe Bool)
isUserAdmin uid = do
  withConn $ Session.run (Session.statement (fromIntegral uid) stmt)
  where
    stmt :: Statement Int32 (Maybe Bool)
    stmt =
      [TH.maybeStatement|
        select ("is_admin" :: bool) from "user" where "id" = ($1 :: int4)
      |]

insertUser
  :: (WithDb, HasCallStack)
  => T.Text
  -> BS.ByteString
  -> IO (Maybe Int)
insertUser username hashedPassword = do
  fmap fromIntegral <$> withConn (Session.run (Session.statement (username, hashedPassword) stmt))
  where
    stmt :: Statement (T.Text, BS.ByteString) (Maybe Int32)
    stmt =
      [TH.maybeStatement|
        insert into "user" ("username", "password", "is_admin")
         values ($1 :: text, $2 :: bytea, false)
         on conflict do nothing
         returning "id" :: int4
      |]

findUserByUsername
  :: (WithDb, HasCallStack)
  => T.Text
  -> IO (Maybe (Int, BS.ByteString))
findUserByUsername username = do
  fmap (first fromIntegral) <$> withConn (Session.run (Session.statement username stmt))
  where
    stmt :: Statement T.Text (Maybe (Int32, BS.ByteString))
    stmt =
      [TH.maybeStatement|
        select ("id" :: int4), ("password" :: bytea) from "user" 
        where "username" = ($1 :: text)
      |]