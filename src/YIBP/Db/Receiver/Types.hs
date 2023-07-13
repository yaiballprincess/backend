module YIBP.Db.Receiver.Types where

import Data.Text qualified as T

import YIBP.Core.Sender
import YIBP.Core.Id

data InsertReceiver = InsertReceiver
  { senderId :: Id SenderTag
  , name :: !T.Text
  , peerId :: !Int
  }

data RawReceiver = RawReceiver
  { senderId :: Id SenderTag
  , name :: !T.Text
  , peerId :: !Int
  }
