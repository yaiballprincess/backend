module YIBP.Core.Auth (AuthData (..), withAuth) where

import GHC.Generics

import YIBP.Core.Id
import YIBP.Core.User

import Data.Aeson

import Servant
import Servant.Auth.Server

newtype AuthData = AuthData {userId :: Id User}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, FromJWT, ToJWT)

withAuth :: (ThrowAll a) => AuthResult AuthData -> a -> a
withAuth (Authenticated _) a = a
withAuth _ _ = throwAll err403
