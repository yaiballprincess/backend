{-# LANGUAGE OverloadedStrings #-}

module YIBP.Db.Sender
  ( insertSender
  , deleteSender
  , getSendersTrimmed
  , getSendersTrimmedWithReceivers
  , getSenderById
  ) where

import Hasql.Decoders qualified as D
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement

import YIBP.Core.Id
import YIBP.Core.Sender
import YIBP.Db
import YIBP.Db.Id.Decoders
import YIBP.Db.Id.Encoders
import YIBP.Db.Sender.Decoders
import YIBP.Db.Sender.Encoders
import YIBP.Db.Sender.Types

import Data.Vector qualified as V

insertSender :: (WithDb) => InsertSender -> IO (Maybe (Id SenderTag))
insertSender is = withConn $ Session.run (Session.statement is stmt)
  where
    stmt =
      Statement
        "insert into \"sender\" \
        \ (user_id, name, access_token_enc, bot_access_token_enc, bot_id) \
        \ values ($1, $2, $3, $4, $5) \
        \ on conflict do nothing \
        \ returning id"
        insertSenderParams
        (Decoders.rowMaybe idRow)
        True

deleteSender :: (WithDb) => Id SenderTag -> IO Bool
deleteSender sId = withConn $ Session.run ((== 1) <$> Session.statement sId stmt)
  where
    stmt =
      Statement
        "delete from \"sender\" where id = $1"
        idParams
        Decoders.rowsAffected
        True

getSendersTrimmed :: (WithDb) => IO (V.Vector RawSenderTrimmed)
getSendersTrimmed = withConn $ Session.run (Session.statement () stmt)
  where
    stmt =
      Statement
        "select id, user_id, name, bot_id from \"sender\""
        Encoders.noParams
        (Decoders.rowVector senderTrimmedRow)
        True

getSendersTrimmedWithReceivers :: (WithDb) => IO (V.Vector RawSenderTrimmedWithReceivers)
getSendersTrimmedWithReceivers = withConn $ Session.run (Session.statement () stmt)
  where
    stmt =
      Statement
        "select id, user_id, sender.name, bot_id, \
        \ array_remove(array_agg(r.name), NULL), \
        \ array_remove(array_agg(r.peer_id), NULL) from \"sender\" \
        \ left join \"receiver\" r ON r.sender_id = sender.id \
        \ group by sender.id"
        Encoders.noParams
        (Decoders.rowVector decoder)
        True
    decoder =
      RawSenderTrimmedWithReceivers
        <$> idRow
        <*> D.column (D.nonNullable (fromIntegral <$> D.int4))
        <*> D.column (D.nonNullable D.text)
        <*> D.column (D.nullable (fromIntegral <$> D.int4))
        <*> D.column (D.nonNullable (D.vectorArray (D.nonNullable D.text)))
        <*> D.column (D.nonNullable (D.vectorArray (D.nonNullable (fromIntegral <$> D.int4))))

getSenderById :: (WithDb) => Id SenderTag -> IO (Maybe RawSender)
getSenderById senderId = withConn $ Session.run (Session.statement senderId stmt)
  where
    stmt =
      Statement
        "select id, name, access_token_enc, bot_access_token_enc, bot_id \
        \ from \"sender\" where id = $1"
        idParams
        (Decoders.rowMaybe senderRow)
        True
