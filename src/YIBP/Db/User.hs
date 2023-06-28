{-# LANGUAGE OverloadedStrings #-}

module YIBP.Db.User (getUserById, getUserByUsername, insertUser, findUserByUsername) where

import Data.ByteString qualified as BS
import Data.Int
import Data.Text qualified as T

import GHC.Stack (HasCallStack)

import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH

import Data.Bifunctor
import YIBP.Core.Id
import YIBP.Core.User
import YIBP.Db
import YIBP.Db.Id.Decoders
import YIBP.Db.Id.Encoders
import YIBP.Db.User.Decoders
import YIBP.Db.User.Encoders
import YIBP.Db.User.Types

getUserById :: (WithDb, HasCallStack) => Id User -> IO (Maybe RawUser)
getUserById uid = withConn (Session.run (Session.statement uid stmt))
  where
    stmt =
      Statement
        "select id, username, password, is_admin from \"user\" where id = $1"
        idParams
        (Decoders.rowMaybe userRow)
        True

getUserByUsername :: (WithDb, HasCallStack) => T.Text -> IO (Maybe RawUser)
getUserByUsername username = withConn (Session.run (Session.statement username stmt))
  where
    stmt =
      Statement
        "select id, username, password, is_admin from \"user\" where username = $1"
        (Encoders.param (Encoders.nonNullable Encoders.text))
        (Decoders.rowMaybe userRow)
        True

insertUser
  :: (WithDb, HasCallStack)
  => InsertUser
  -> IO (Maybe (Id User))
insertUser iu = do
  withConn (Session.run (Session.statement iu stmt))
  where
    stmt =
      Statement
        "insert into \"user\" (username, password, is_admin) \
        \values ($1, $2, $3) \
        \on conflict do nothing \
        \returning id"
        insertUserParams
        (Decoders.rowMaybe idRow)
        True

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
