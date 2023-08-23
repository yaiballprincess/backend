module YIBP.Core.Sender
  ( SenderTag
  , BotSender (..)
  , Sender (..)
  , InsertSender (..)
  , CreateSenderParam (..)
  , SenderTrimmed (..)
  , SenderTrimmedWithReceivers (..)
  , getSenderToken
  ) where

import YIBP.Crypto

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)

import YIBP.Core.Receiver

data SenderTag

data BotSender (s :: EncryptionStatus) = BotSender
  { accessToken :: !(CryptoText s)
  , id :: !Int
  }

deriving instance Show (BotSender 'Decrypted)
deriving instance Show (BotSender 'Encrypted)

data Sender (s :: EncryptionStatus) = Sender
  { name :: !T.Text
  , accessToken :: !(CryptoText s)
  , bot :: !(Maybe (BotSender s))
  }

deriving instance Show (Sender 'Decrypted)
deriving instance Show (Sender 'Encrypted)

data InsertSender = InsertSender
  { userId :: !Int
  , name :: !T.Text
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

data SenderTrimmedWithReceivers = SenderTrimmedWithReceivers
  { id :: !Int
  , name :: !T.Text
  , botName :: !(Maybe T.Text)
  , receivers :: !(V.Vector Receiver)
  }
  deriving (Generic, FromJSON, ToJSON)

getSenderToken :: Sender 'Decrypted -> T.Text
getSenderToken s = case s.bot of
  Just x -> x.accessToken
  Nothing -> s.accessToken
