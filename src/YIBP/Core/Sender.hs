module YIBP.Core.Sender
  ( SenderTag
  , BotSender (..)
  , Sender (..)
  , InsertSender (..)
  , CreateSenderParam (..)
  , SenderTrimmed (..)
  ) where

import YIBP.Crypto

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)

data SenderTag

data BotSender (s :: EncryptionStatus) = BotSender
  { accessToken :: !(CryptoText s)
  , id :: !Int
  }

data Sender (s :: EncryptionStatus) = Sender
  { name :: !T.Text
  , accessToken :: !(CryptoText s)
  , bot :: !(Maybe (BotSender s))
  }

data InsertSender = InsertSender
  { name :: !T.Text
  , accessToken :: !(CryptoText 'Encrypted)
  , bot :: !(Maybe (BotSender 'Encrypted))
  }

data CreateSenderParam = CreateSenderParam
  { accessToken :: !T.Text
  , botAccessToken :: !(Maybe T.Text)
  }
  deriving (Generic, FromJSON, ToJSON)

data SenderTrimmed = SenderTrimmed
  { id :: !Int
  , name :: !T.Text
  , botName :: !(Maybe T.Text)
  }
  deriving (Generic, FromJSON, ToJSON)
