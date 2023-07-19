{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.Session (SessionAPI, theSessionAPI) where

import Control.Monad.Catch (catch)
import Control.Monad.Except
import Data.Text qualified as T
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Web.Cookie
import YIBP.Config
import YIBP.Core.Session
import YIBP.Core.User
import YIBP.Db
import YIBP.Error
import YIBP.Service.Session
import YIBP.Validate

data SessionAPI route = SessionAPI
  { _login
      :: route
        :- ReqBody '[JSON] LoginUserParam
          :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] AccessToken)
  , _refresh
      :: route
        :- Header "Cookie" RefreshToken
          :> Put '[JSON] (Headers '[Header "Set-Cookie" SetCookie] AccessToken)
  }
  deriving (Generic)

theSessionAPI :: (WithDb, WithConfig) => SessionAPI (AsServerT Handler)
theSessionAPI =
  SessionAPI
    { _login = loginHandler
    , _refresh = refreshHandler
    }

loginHandler :: (WithDb, WithConfig) => LoginUserParam -> Handler (Headers '[Header "Set-Cookie" SetCookie] AccessToken)
loginHandler luParam = do
  lu <- validateOrThrowServerError $ validateLoginUser luParam
  (cookie, accessToken) <-
    liftIO (doLogin lu)
      `catch` (\UserNotFound -> raiseServantError (HttpError @UserNotFound "user not found") err422)
      `catch` (\InvalidPassword -> raiseServantError (HttpError @InvalidPassword "invalid password") err422)
  pure $ addHeader cookie accessToken

refreshHandler :: (WithDb, WithConfig) => Maybe RefreshToken -> Handler (Headers '[Header "Set-Cookie" SetCookie] AccessToken)
refreshHandler Nothing = throwError err403
refreshHandler (Just refreshToken) = do
  (cookie, accessToken) <-
    liftIO (doRefresh refreshToken)
      `catch` (\RefreshTokenIsInvalid -> raiseServantError (HttpError @RefreshTokenIsInvalid "refresh token is invalid") err403)
  pure $ addHeader cookie accessToken
