{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.User where

import Servant
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Servant.Server.Generic

import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import YIBP.Config
import YIBP.Core.Auth
import YIBP.Core.Id
import YIBP.Core.User
import YIBP.Db
import YIBP.Error
import YIBP.Service.User
import YIBP.Validate

data UserAPI route = UserAPI
  { _register
      :: route
        :- Auth '[JWT] AuthData
        :> ReqBody '[JSON] CreateUserParam
        :> Post '[JSON] (Id User)
  }
  deriving (Generic)

theUserAPI :: (WithDb, WithConfig) => UserAPI (AsServerT Handler)
theUserAPI =
  UserAPI
    { _register = registerHandler
    }

registerHandler
  :: (WithDb, WithConfig)
  => AuthResult AuthData
  -> CreateUserParam
  -> Handler (Id User)
registerHandler (Authenticated authData) cuParam = do
  cu <- validateOrThrowServerError $ validateCreateUser cuParam
  liftIO (registerUser authData cu)
    `catch` (\NotEnoughPermissionsError -> raiseServantError (HttpError @NotEnoughPermissionsError "not enough permissions") err403)
    `catch` (\UserConflictError -> raiseServantError (HttpError @UserConflictError "user exists") err409)
registerHandler _ _ = throwError err400
