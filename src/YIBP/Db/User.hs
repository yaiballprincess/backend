module YIBP.Db.User (isUserAdmin, insertUser, findUserByUsernameAndPassword) where

import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T

import GHC.Stack (HasCallStack)

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH

import YIBP.Db.Util

isUserAdmin
  :: (WithDb env m, HasCallStack)
  => Int
  -> m (Maybe Bool)
isUserAdmin uid = do
  r <- withConn $ Session.run (Session.statement (fromIntegral uid) stmt)
  liftError r
  where
    stmt :: Statement Int32 (Maybe Bool)
    stmt =
      [TH.maybeStatement|
        select ("is_admin" :: bool) from "user" where "id" = ($1 :: int4)
      |]

insertUser
  :: (WithDb env m, HasCallStack)
  => T.Text
  -> BS.ByteString
  -> m (Maybe Int)
insertUser username hashedPassword = do
  r <- withConn $ Session.run (Session.statement (username, hashedPassword) stmt)
  liftError (fmap (fmap fromIntegral) r)
  where
    stmt :: Statement (T.Text, BS.ByteString) (Maybe Int32)
    stmt =
      [TH.maybeStatement|
        insert into "user" ("username", "password", "is_admin")
         values ($1 :: text, $2 :: bytea, false)
         on conflict do nothing
         returning "id" :: int4
      |]

findUserByUsernameAndPassword
  :: (WithDb env m, HasCallStack)
  => T.Text
  -> BS.ByteString
  -> m (Maybe Int)
findUserByUsernameAndPassword username hashedPassword = do
  r <- withConn $ Session.run (Session.statement (username, hashedPassword) stmt)
  liftError (fmap (fmap fromIntegral) r)
  where
    stmt :: Statement (T.Text, BS.ByteString) (Maybe Int32)
    stmt =
      [TH.maybeStatement|
        select ("id" :: int4) from "user" 
        where "username" = ($1 :: text) and  "password" = ($2 :: bytea)
      |]