{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.Auth (AuthAPI (..), theAuthAPI) where

import Control.Monad.IO.Class

import Servant
import Servant.API
import Servant.API.Generic
import Servant.Auth
import Servant.Server
import Servant.Server.Generic

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time qualified as Time
import Data.UUID
import Data.UUID.V4

import Optics

import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader.Class (MonadReader, asks)
import Crypto.JOSE
import Servant.Auth.Server
import YIBP.App
import YIBP.Core.User qualified as C
import YIBP.Db.Auth
import YIBP.Db.User
import YIBP.Db.Util
import YIBP.Util

newtype AuthData = AuthData {uid :: Int}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, FromJWT, ToJWT)

makeFieldsNoPrefix ''AuthData

data LoginRequest = LoginRequest
  { username :: !T.Text
  , password :: !T.Text
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

makeFieldsNoPrefix ''LoginRequest

newtype RefreshTokenRequest = RefreshTokenRequest UUID
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data RefreshTokenResponse = RefreshTokenResponse
  { refreshToken :: !UUID
  , accessToken :: !T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeFieldsNoPrefix ''RefreshTokenRequest
makeFieldsNoPrefix ''RefreshTokenResponse

data AuthAPI route = AuthAPI
  { _register
      :: route
        :- Auth '[JWT] AuthData
        :> "register"
        :> ReqBody '[JSON] LoginRequest
        :> Post '[JSON] (Maybe Int)
  , _login
      :: route
        :- "login"
        :> ReqBody '[JSON] LoginRequest
        :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
  , _refresh
      :: route
        :- "refresh"
        :> ReqBody '[JSON] RefreshTokenRequest
        :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
  }
  deriving (Generic)

theAuthAPI :: (MonadIO m, MonadError ServerError m) => AuthAPI (AsServerT (AppT m))
theAuthAPI =
  AuthAPI
    { _register = registerHandler
    , _login = loginHandler
    , _refresh = refreshHandler
    }

registerHandler
  :: (MonadIO m, WithDb env m, MonadError ServerError m)
  => AuthResult AuthData
  -> LoginRequest
  -> m (Maybe Int)
registerHandler authData req = do
  b <- isAdmin authData
  unless b $ do
    throwError err403
  when (C.checkUsername (req ^. #username)) $ do
    throwError $ err422 {errBody = "invalid username"}
  when (C.checkPassword (req ^. #password)) $ do
    throwError $ err422 {errBody = "invalid password"}
  PasswordHash hashedPassword <- hashPassword $ mkPassword $ req ^. #password
  insertUser (req ^. #username) (T.encodeUtf8 hashedPassword)

loginHandler
  :: ( MonadIO m
     , WithDb env m
     , MonadReader env m
     , Has JWK env
     , MonadError ServerError m
     )
  => LoginRequest
  -> m (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
loginHandler req = do
  PasswordHash hashedPassword <- hashPassword $ mkPassword $ req ^. #password
  mb <- findUserByUsernameAndPassword (req ^. #username) (T.encodeUtf8 hashedPassword)
  case mb of
    Nothing -> throwError err403
    Just uid -> do
      randomUUID <- liftIO nextRandom
      mtime <- insertRefreshToken randomUUID uid
      case mtime of
        Nothing -> throwError err403
        Just time -> do
          cookie <-
            liftIO $
              makeXsrfCookie $
                defaultCookieSettings
                  { cookiePath = Just "/api/auth"
                  , cookieExpires = Just $ Time.addUTCTime (60 * Time.nominalDay) time -- TODO: use from settings
                  , sessionCookieName = "MyRefreshToken"
                  }
          myJWK <- asks $ obtain @JWK
          ejwt <-
            liftIO $
              makeJWT
                (AuthData uid)
                (defaultJWTSettings myJWK)
                (Just $ Time.addUTCTime (60 * 60) time)
          case ejwt of
            Left _ -> throwError err500
            Right jwt ->
              pure $
                addHeader cookie $
                  RefreshTokenResponse
                    { refreshToken = randomUUID
                    , accessToken = T.decodeUtf8 $ BS.toStrict jwt
                    }

refreshHandler
  :: (MonadIO m)
  => RefreshTokenRequest
  -> m (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
refreshHandler req = undefined

isAdmin
  :: (MonadIO m, WithDb env m)
  => AuthResult AuthData
  -> m Bool
isAdmin (Authenticated (AuthData uid)) = do
  fromMaybe False <$> isUserAdmin uid
isAdmin _ = pure False