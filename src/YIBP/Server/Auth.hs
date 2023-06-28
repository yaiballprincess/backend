{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.Auth (AuthAPI (..), theAuthAPI, AuthData (..), withAuth, withAuth') where

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
import YIBP.Core.Auth
import YIBP.Core.Id
import YIBP.Core.User qualified as C
import YIBP.Db
import YIBP.Db.Auth
import YIBP.Db.User
import YIBP.Util

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
  { _login
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

theAuthAPI :: (WithDb, WithConfig) => AuthAPI (AsServerT Handler)
theAuthAPI =
  AuthAPI
    { _login = loginHandler
    , _refresh = refreshHandler
    }

loginHandler
  :: (WithDb, WithConfig)
  => LoginRequest
  -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
loginHandler req = do
  let password = mkPassword (req ^. #password)
  mb <- liftIO $ findUserByUsername (req ^. #username)
  (uid, hashedPassword) <- whenNothing mb $ throwError err403
  when (checkPassword password (PasswordHash (T.decodeUtf8 hashedPassword)) == PasswordCheckFail) $ do
    throwError err403
  randomUUID <- liftIO nextRandom
  mtime <- liftIO $ insertRefreshToken randomUUID uid
  time <- whenNothing mtime $ throwError err403
  (cookie, t) <- genCookieAndJWT randomUUID uid time
  pure $ addHeader cookie $ RefreshTokenResponse t

refreshHandler
  :: (WithDb, WithConfig)
  => Maybe T.Text
  -> Handler (Headers '[Header "Set-Cookie" SetCookie] RefreshTokenResponse)
refreshHandler (Just rawCookieText) = do
  let maybeRefreshToken = fromText . snd =<< find (\(key, _) -> key == "MyRefreshToken") (parseCookiesText $ T.encodeUtf8 rawCookieText)
  token <- whenNothing maybeRefreshToken $ throwError err403
  createdAt <- liftIO (getCreatedAtByRefreshToken token) >>= flip whenNothing (throwError err403)
  currTime <- liftIO Time.getCurrentTime
  when (Time.diffUTCTime currTime createdAt >= 60 * Time.nominalDay) $ throwError err403
  randomUUID <- liftIO nextRandom
  (newCreatedAt, uid) <- liftIO (updateRefreshToken token randomUUID) >>= flip whenNothing (throwError err403)
  (cookie, t) <- genCookieAndJWT randomUUID uid newCreatedAt
  pure $ addHeader cookie $ RefreshTokenResponse t
refreshHandler _ = throwError err403

genCookieAndJWT
  :: (WithConfig)
  => UUID
  -> Int
  -> Time.UTCTime
  -> Handler (SetCookie, T.Text)
genCookieAndJWT uuid ownerId createdAt = do
  let theConfig = getConfig
  let refreshTokenDuration = theConfig.refreshTokenDuration
  let accessTokenDuration = theConfig.accessTokenDuration
  let cookie =
        defaultSetCookie
          { setCookieName = "MyRefreshToken"
          , setCookieValue = T.encodeUtf8 $ toText uuid
          , setCookieExpires = Just $ Time.addUTCTime refreshTokenDuration createdAt
          , setCookiePath = Just "/api/auth"
          , setCookieHttpOnly = True
          }
  ejwt <-
    liftIO $
      makeJWT
        (AuthData (Id ownerId))
        (defaultJWTSettings theConfig.jwk)
        (Just $ Time.addUTCTime accessTokenDuration createdAt)
  case ejwt of
    Left _ -> throwError err500
    Right jwt ->
      pure (cookie, T.decodeUtf8 $ BS.toStrict jwt)

withAuth :: (MonadError ServerError m) => AuthResult AuthData -> m a -> m a
withAuth (Authenticated _) m = m
withAuth _ _ = throwError err403

withAuth' :: (ThrowAll a) => AuthResult AuthData -> a -> a
withAuth' (Authenticated _) a = a
withAuth' _ _ = throwAll err403
