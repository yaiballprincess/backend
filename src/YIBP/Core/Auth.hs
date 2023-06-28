module YIBP.Core.Auth (AuthData (..)) where

import GHC.Generics

import YIBP.Core.Id
import YIBP.Core.User

import Data.Aeson

import Servant.Auth.Server

newtype AuthData = AuthData { userId :: Id User }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, FromJWT, ToJWT)
