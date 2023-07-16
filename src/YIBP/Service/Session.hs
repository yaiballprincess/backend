{-# LANGUAGE OverloadedStrings #-}

module YIBP.Service.Session where

import Data.ByteString qualified as BS
import Data.Password.Bcrypt qualified as BCrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Data.UUID.V4

import YIBP.Config
import YIBP.Core.Session
import YIBP.Core.User
import YIBP.Db
import YIBP.Db.Session.Types

import Control.Exception
import Control.Monad
import Crypto.JOSE
import GHC.Stack
import Servant.Auth.Server
import Web.Cookie
import YIBP.Core.Auth
import YIBP.Core.Id
import YIBP.Db.Session
import YIBP.Db.User
import YIBP.Db.User.Types

data UserNotFound = UserNotFound
  deriving (Show, Eq, Exception)

data InvalidPassword = InvalidPassword
  deriving (Show, Eq, Exception)

generateRefreshToken :: IO RefreshToken
generateRefreshToken = RefreshToken <$> nextRandom

createRefreshTokenCookie :: RefreshToken -> UTCTime -> SetCookie
createRefreshTokenCookie (RefreshToken uuid) expiresAt =
  defaultSetCookie
    { setCookieName = refreshTokenCookieName
    , setCookieValue = T.encodeUtf8 (UUID.toText uuid)
    , setCookieExpires = Just expiresAt
    , setCookieHttpOnly = True
    , setCookiePath = Just "/api/sessions"
    }

generateAccessToken :: JWK -> Id User -> UTCTime -> IO AccessToken
generateAccessToken theJwk uid expiresAt = do
  makeJWT
    (AuthData uid)
    (defaultJWTSettings theJwk)
    (Just expiresAt)
    >>= \case
      Left e -> error $ "failed to generate access token: " <> show e
      Right bs -> pure $ AccessToken (T.decodeUtf8 $ BS.toStrict bs)

passwordIsCorrect :: BS.ByteString -> T.Text -> Bool
passwordIsCorrect hashed password = case result of
  BCrypt.PasswordCheckSuccess -> True
  BCrypt.PasswordCheckFail -> False
  where
    result =
      BCrypt.checkPassword
        (BCrypt.mkPassword password)
        (BCrypt.PasswordHash (T.decodeUtf8 hashed))

generateTokensPair :: (WithConfig, HasCallStack) => UTCTime -> Id User -> IO (RefreshToken, SetCookie, AccessToken)
generateTokensPair curTime uid = do
  let accessTokenExpirationTime = addUTCTime cfg.accessTokenDuration curTime
  let refreshTokenExpirationTime = addUTCTime cfg.refreshTokenDuration curTime
  accessToken <- generateAccessToken cfg.jwk uid accessTokenExpirationTime
  refreshToken <- generateRefreshToken
  let cookie = createRefreshTokenCookie refreshToken refreshTokenExpirationTime
  pure (refreshToken, cookie, accessToken)
  where
    cfg = withConfig id

doLogin :: (WithDb, WithConfig, HasCallStack) => LoginUser -> IO (SetCookie, AccessToken)
doLogin lu = do
  getUserByUsername lu.username >>= \case
    Nothing -> throwIO UserNotFound
    Just u -> do
      unless (passwordIsCorrect u.hashedPassword lu.password) $ do
        throwIO InvalidPassword
      let uid = Id u.id
      curTime <- getCurrentTime
      (refreshToken, refreshTokenCookie, accessToken) <- generateTokensPair curTime uid
      _ <- insertRefreshToken $ InsertSession {refreshToken = refreshToken, ownerId = uid, createdAt = curTime}
      pure (refreshTokenCookie, accessToken)

data RefreshTokenIsInvalid = RefreshTokenIsInvalid
  deriving (Show, Eq, Exception)

doRefresh :: (WithDb, WithConfig, HasCallStack) => RefreshToken -> IO (SetCookie, AccessToken)
doRefresh refreshToken = do
  updateRefreshTokenWithValidationCheck (fromRational . toRational $ cfg.refreshTokenDuration) refreshToken >>= \case
    Nothing -> throwIO RefreshTokenIsInvalid
    Just newSession -> do
      let accessTokenExpirationTime = addUTCTime cfg.accessTokenDuration newSession.createdAt
      let refreshTokenExpirationTime = addUTCTime cfg.refreshTokenDuration newSession.createdAt
      accessToken <- generateAccessToken cfg.jwk newSession.ownerId accessTokenExpirationTime
      pure (createRefreshTokenCookie newSession.refreshToken refreshTokenExpirationTime, accessToken)
  where
    cfg = withConfig id
