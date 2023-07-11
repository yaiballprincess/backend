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
    <*> column (nullable (composite botSenderComposite))
  where
    botSenderComposite :: Composite RawBotSender
    botSenderComposite =
      RawBotSender
        <$> field (nonNullable bytea)
        <*> fmap fromIntegral (field (nonNullable int4))

senderTrimmedRow :: Row RawSenderTrimmed
senderTrimmedRow =
  RawSenderTrimmed
    <$> idRow
    <*> column (nonNullable text)
    <*> column (nullable (fromIntegral <$> int4))
