module YIBP.Core.Receiver (Receiver (..), CreateReceiverParam (..)) where

import Data.Text qualified as T
import Data.Aeson
import GHC.Generics

data Receiver = Receiver
  { name :: !T.Text
  , peerId :: !Int
  }

data CreateReceiverParam = CreateReceiverParam
  { name :: !T.Text
  , peerId :: !Int
  } deriving (Generic, FromJSON, ToJSON)
