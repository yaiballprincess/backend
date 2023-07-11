module YIBP.Db.Sender.Types
  ( RawBotSender (..)
  , RawSender (..)
  , RawSenderTrimmed (..)
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T

import YIBP.Core.Id (Id)
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
  , name :: !T.Text
  , botId :: !(Maybe Int)
  }
