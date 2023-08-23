module YIBP.Db.Sender.Encoders where

import YIBP.Core.Sender

import Data.Functor.Contravariant
import Hasql.Encoders

insertSenderParams :: Params InsertSender
insertSenderParams =
  ((\s -> fromIntegral s.userId) >$< param (nonNullable int4))
    <> ((\s -> s.name) >$< param (nonNullable text))
    <> ((\s -> s.accessToken) >$< param (nonNullable bytea))
    <> ((\s -> (.accessToken) <$> s.bot) >$< param (nullable bytea))
    <> ((\s -> fromIntegral . (.id) <$> s.bot) >$< param (nullable int4))
