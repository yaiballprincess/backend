{-# LANGUAGE OverloadedStrings #-}

module YIBP.Db.Receiver where

import YIBP.Db

import YIBP.Core.Id
import YIBP.Core.Sender

import YIBP.Db.Id.Encoders

import YIBP.Db.Receiver.Decoders
import YIBP.Db.Receiver.Encoders
import YIBP.Db.Receiver.Types

import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Session qualified as Session
import Hasql.Statement

import Contravariant.Extras.Contrazip
import Data.Functor.Contravariant

insertReceiver :: (WithDb) => InsertReceiver -> IO Bool
insertReceiver ir = withConn $ Session.run ((== 1) <$> Session.statement ir stmt)
  where
    stmt =
      Statement
        "insert into \"receiver\" (sender_id, name, peer_id) values \
        \ ($1, $2, $3) on conflict do nothing"
        insertReceiverParams
        D.rowsAffected
        True

deleteReceiver :: (WithDb) => Id SenderTag -> Int -> IO Bool
deleteReceiver senderId peerId = withConn $ Session.run ((== 1) <$> Session.statement (senderId, peerId) stmt)
  where
    stmt =
      Statement
        "delete from \"receiver\" where sender_id = $1 and peer_id = $2"
        (contrazip2 idParams (fromIntegral >$< E.param (E.nonNullable E.int4)))
        D.rowsAffected
        True
