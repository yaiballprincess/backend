{-# LANGUAGE OverloadedStrings #-}
module YIBP.Core.Id where

import GHC.Generics
import Data.Aeson
import Data.Aeson.KeyMap qualified as J
import Servant

newtype Id a = Id Int
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

data IdObject a = IdObject (Id a) a
  deriving (Show, Eq)

instance ToJSON a => ToJSON (IdObject a) where
  toJSON (IdObject oId obj) = case toJSON obj of
    Object o -> toJSON $ J.insert "id" (toJSON oId) o
    other -> object [ "id" .= oId, "value" .= other ]
