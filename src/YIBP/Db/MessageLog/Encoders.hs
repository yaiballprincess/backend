module YIBP.Db.MessageLog.Encoders where

import YIBP.Core.MessageLog

import Data.Functor.Contravariant
import Hasql.Encoders

messageLogParams :: Params MessageLog
messageLogParams =
  ((.sentAt) >$< param (nonNullable undefined))
    <> ((maybeMessageLogError . (.result)) >$< param (nullable text))
    <> ((maybeMessageLogOk . (.result)) >$< param (nullable text))
  where
    maybeMessageLogOk (MessageLogOk t) = Just t
    maybeMessageLogOk _ = Nothing

    maybeMessageLogError (MessageLogError t) = Just t
    maybeMessageLogError _ = Nothing
