module YIBP.Db.Sender.Types
  ( RawBotSender (..)
  , RawSender (..)
  , RawSenderTrimmed (..)
  , RawSenderTrimmedWithReceivers (..)
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V

import YIBP.Core.Id (Id)
import YIBP.Core.Receiver (Receiver)
import YIBP.Core.Sender (SenderTag)

data RawBotSender = RawBotSender
  { accessToken :: !BS.ByteString
  , id :: !Int
  }

data RawSender = RawSender
  { id :: !(Id SenderTag)
  , name :: !T.Text
  , accessToken :: !BS.ByteString
  , bot :: !(Maybe RawBotSender)
  }

data RawSenderTrimmed = RawSenderTrimmed
  { id :: !(Id SenderTag)
  , userId :: !Int
  , name :: !T.Text
  , botId :: !(Maybe Int)
  }

data RawSenderTrimmedWithReceivers = RawSenderTrimmedWithReceivers
  { id :: !(Id SenderTag)
  , userId :: !Int
  , name :: !T.Text
  , botId :: !(Maybe Int)
  , receiverNames :: !(V.Vector T.Text)
  , receiverPeerIds :: !(V.Vector Int)
  }
