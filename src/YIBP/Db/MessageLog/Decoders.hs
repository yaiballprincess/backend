module YIBP.Db.MessageLog.Decoders where

import YIBP.Core.MessageLog
import YIBP.Db.MessageLog.Types

import Hasql.Decoders
import YIBP.Db.Id.Decoders

messageLogRow :: Row RawMessageLog
messageLogRow =
  RawMessageLog
    <$> idRow
    <*> (MessageLog <$> column (nonNullable timestamptz) <*> pr)
  where
    pr =
      column (nullable text) >>= \case
        Nothing -> MessageLogOk <$> column (nonNullable text)
        Just t -> column (nullable text) >> pure (MessageLogError t)
