module YIBP.Db.Sender.Decoders where

import YIBP.Db.Id.Decoders
import YIBP.Db.Sender.Types

import Hasql.Decoders

senderRow :: Row RawSender
senderRow =
  RawSender
    <$> idRow
    <*> column (nonNullable text)
    <*> column (nonNullable bytea)
    <*> botSenderRow
  where
    botSenderRow :: Row (Maybe RawBotSender)
    botSenderRow = do
      accessToken <- column (nullable bytea)
      theId <- fmap fromIntegral <$> column (nullable int4)
      pure $ RawBotSender <$> accessToken <*> theId

senderTrimmedRow :: Row RawSenderTrimmed
senderTrimmedRow =
  RawSenderTrimmed
    <$> idRow
    <*> column (nonNullable text)
    <*> column (nullable (fromIntegral <$> int4))
