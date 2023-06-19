{-# LANGUAGE DeriveAnyClass #-}

module YIBP.Core.Sender where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics

import Optics

data Sender = Sender
  { name :: !T.Text
  , accessToken :: !T.Text
  , botAccessToken :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeFieldsNoPrefix ''Sender