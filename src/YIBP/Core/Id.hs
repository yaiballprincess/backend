{-# LANGUAGE OverloadedStrings #-}

module YIBP.Core.Id where

import Data.Aeson
import Data.Aeson.KeyMap qualified as J
import GHC.Generics
import Servant

newtype Id a = Id Int
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

idToInt :: Id a -> Int
idToInt (Id x) = x

data IdObject a = IdObject
  { id :: !(Id a)
  , value :: !a
  }
  deriving (Show, Eq)

instance (ToJSON a) => ToJSON (IdObject a) where
  toJSON (IdObject oId obj) = case toJSON obj of
    Object o -> toJSON $ J.insert "id" (toJSON oId) o
    other -> object ["id" .= oId, "value" .= other]
