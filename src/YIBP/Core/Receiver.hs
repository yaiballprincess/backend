module YIBP.Core.Receiver (Receiver (..), CreateReceiverParam (..), PeerIdParam (..)) where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics
import Servant

data Receiver = Receiver
  { name :: !T.Text
  , peerId :: !Int
  }
  deriving (Generic, FromJSON, ToJSON)

newtype CreateReceiverParam = CreateReceiverParam
  { peerId :: Int
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype PeerIdParam = PeerIdParam Int
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)
