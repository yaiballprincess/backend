module YIBP.Db.Receiver.Decoders where

import YIBP.Core.Receiver
import YIBP.Db.Id.Decoders
import YIBP.Db.Receiver.Types

import Hasql.Decoders

receiverRow :: Row Receiver
receiverRow =
  Receiver
    <$> column (nonNullable text)
    <*> fmap fromIntegral (column (nonNullable int4))

rawReceiverRow :: Row RawReceiver
rawReceiverRow =
  RawReceiver
    <$> idRow
    <*> column (nonNullable text)
    <*> fmap fromIntegral (column (nonNullable int4))
