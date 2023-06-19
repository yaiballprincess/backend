{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.Auth (AuthAPI (..), theAuthAPI, AuthData (..), withAuth) where

import Control.Monad.IO.Class

import Web.Cookie

import Servant
import Servant.API.Generic
import Servant.Auth
import Servant.Server.Generic

import Data.Aeson
import Data.ByteString qualified as BS
import Data.List
import Data.Maybe
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time qualified as Time
import Data.UUID
import Data.UUID.V4

import Optics

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader
import Crypto.JOSE
import Servant.Auth.Server
import YIBP.App
import YIBP.Config
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

newtype RefreshTokenResponse = RefreshTokenResponse
  {accessToken :: T.Text}
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
        :> Header "Cookie" T.Text
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
  unless (C.checkUsername (req ^. #username)) $ do
    throwError $ err422 {errBody = "invalid username"}
  unless (C.checkPassword (req ^. #password)) $ do
    throwError $ err422 {errBody = "invalid password"}
  PasswordHash hashedPassword <- hashPassword $ mkPassword $ req ^. #password
  insertUser (req ^. #username) (T.encodeUtf8 hashedPassword)

loginHandler
  :: ( MonadIO m
     , WithDb env m
     , MonadReader env m
     , Has JWK env
     , Has Config env
     , MonadError ServerError m
     )
  => LoginRequest
  -> m (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
loginHandler req = do
  let password = mkPassword (req ^. #password)
  mb <- findUserByUsername (req ^. #username)
  (uid, hashedPassword) <- whenNothing mb $ throwError err403
  when (checkPassword password (PasswordHash (T.decodeUtf8 hashedPassword)) == PasswordCheckFail) $ do
    throwError err403
  randomUUID <- liftIO nextRandom
  mtime <- insertRefreshToken randomUUID uid
  time <- whenNothing mtime $ throwError err403
  (cookie, t) <- genCookieAndJWT randomUUID uid time
  pure $ addHeader cookie $ RefreshTokenResponse t

refreshHandler
  :: ( MonadIO m
     , MonadError ServerError m
     , WithDb env m
     , MonadReader env m
     , Has Config env
     , Has JWK env
     )
  => Maybe T.Text
  -> m (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
refreshHandler (Just rawCookieText) = do
  let maybeRefreshToken = fromText . snd =<< find (\(key, _) -> key == "MyRefreshToken") (parseCookiesText $ T.encodeUtf8 rawCookieText)
  token <- whenNothing maybeRefreshToken $ throwError err403
  createdAt <- getCreatedAtByRefreshToken token >>= flip whenNothing (throwError err403)
  currTime <- liftIO Time.getCurrentTime
  when (Time.diffUTCTime currTime createdAt >= 60 * Time.nominalDay) $ throwError err403
  randomUUID <- liftIO nextRandom
  (newCreatedAt, uid) <- updateRefreshToken token randomUUID >>= flip whenNothing (throwError err403)
  (cookie, t) <- genCookieAndJWT randomUUID uid newCreatedAt
  pure $ addHeader cookie $ RefreshTokenResponse t
refreshHandler _ = throwError err403

genCookieAndJWT
  :: ( MonadIO m
     , MonadReader env m
     , Has JWK env
     , Has Config env
     , MonadError ServerError m
     )
  => UUID
  -> Int
  -> Time.UTCTime
  -> m (SetCookie, T.Text)
genCookieAndJWT uuid ownerId createdAt = do
  theConfig <- asks $ obtain @Config
  let refreshTokenDuration = theConfig ^. #refreshTokenDuration
  let accessTokenDuration = theConfig ^. #accessTokenDuration
  let cookie =
        defaultSetCookie
          { setCookieName = "MyRefreshToken"
          , setCookieValue = T.encodeUtf8 $ toText uuid
          , setCookieExpires = Just $ Time.addUTCTime refreshTokenDuration createdAt
          , setCookiePath = Just "/api/auth"
          , setCookieHttpOnly = True
          }
  myJWK <- asks $ obtain @JWK
  ejwt <-
    liftIO $
      makeJWT
        (AuthData ownerId)
        (defaultJWTSettings myJWK)
        (Just $ Time.addUTCTime accessTokenDuration createdAt)
  case ejwt of
    Left _ -> throwError err500
    Right jwt ->
      pure (cookie, T.decodeUtf8 $ BS.toStrict jwt)

isAdmin
  :: (MonadIO m, WithDb env m)
  => AuthResult AuthData
  -> m Bool
isAdmin (Authenticated (AuthData uid)) = do
  fromMaybe False <$> isUserAdmin uid
isAdmin _ = pure False

withAuth :: (MonadError ServerError m) => AuthResult AuthData -> m a -> m a
withAuth (Authenticated _) m = m
withAuth _ _ = throwError err403