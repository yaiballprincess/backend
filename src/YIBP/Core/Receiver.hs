{-# LANGUAGE DeriveAnyClass #-}

module YIBP.Core.Receiver where

import Data.Aeson
import Data.Text qualified as T
import GHC.Generics

import Optics

data Receiver = Receiver
  { name :: !T.Text
  , peerId :: !Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeFieldsNoPrefix ''Receiver