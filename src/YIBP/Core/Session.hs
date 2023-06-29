{-# LANGUAGE OverloadedStrings #-}

module YIBP.Core.Session (AccessToken (..), RefreshToken (..), Session (..), refreshTokenCookieName) where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Servant
import Web.Cookie
import YIBP.Core.Id
import YIBP.Core.User

import Fmt

newtype AccessToken = AccessToken T.Text
  deriving newtype (FromJSON, ToJSON)

newtype RefreshToken = RefreshToken UUID
  deriving newtype (FromJSON, ToJSON)

refreshTokenCookieName :: BS.ByteString
refreshTokenCookieName = "YIBP-Refresh-Token"

instance FromHttpApiData RefreshToken where
  parseUrlPiece t = case UUID.fromText t of
    Nothing -> Left "unable to parse RefreshToken"
    Just x -> Right (RefreshToken x)
  parseHeader bs = case find ((== refreshTokenCookieName) . fst) cookies of
    Nothing -> Left $ "no " +| T.decodeUtf8 refreshTokenCookieName |+ " cookie"
    Just (_, rawRefreshToken) -> case UUID.fromText (T.decodeUtf8 rawRefreshToken) of
      Nothing -> Left "unable to parse RefreshToken"
      Just x -> Right (RefreshToken x)
    where
      cookies = parseCookies bs

data Session = Session
  { refreshToken :: !RefreshToken
  , ownerId :: !(Id User)
  , createdAt :: !UTCTime
  }
