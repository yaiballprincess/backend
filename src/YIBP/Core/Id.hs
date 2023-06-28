module YIBP.Core.Id where

import GHC.Generics
import Data.Aeson

newtype Id a = Id Int
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)