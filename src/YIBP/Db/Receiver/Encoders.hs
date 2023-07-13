module YIBP.Db.Receiver.Encoders where

import YIBP.Db.Id.Encoders
import YIBP.Db.Receiver.Types

import Hasql.Encoders

import Data.Functor.Contravariant

insertReceiverParams :: Params InsertReceiver
insertReceiverParams =
  ((.senderId) >$< idParams)
    <> ((.name) >$< param (nonNullable text))
    <> ((fromIntegral . (.peerId)) >$< param (nonNullable int4))
