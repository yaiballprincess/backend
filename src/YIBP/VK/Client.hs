{-# LANGUAGE OverloadedStrings #-}

module YIBP.VK.Client
  ( VKClient
  , VKError (..)
  , mkClient
  , mkDefaultClient
  , mkPair
  , (=--=)
  , sendMethod
  , sendMethodUnsafe
  , WithResponse (..)
  ) where

import Data.Aeson
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Time
import Data.Time.Clock.POSIX

import Data.Function
import Network.Wreq

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson.Optics

import Optics


import Control.Exception (Exception, throwIO)

import GHC.Float.RealFracMethods

data VKClient = VKClient
  { accessToken :: !T.Text
  , version :: !T.Text
  }
  deriving (Eq)

data VKError = VKError
  { errorCode :: !Int
  , errorMsg :: !T.Text
  , requestParams :: !(V.Vector (T.Text, T.Text))
  }
  deriving (Show, Eq, Exception)

instance FromJSON VKError where
  parseJSON = withObject "VKError" $ \o -> do
    VKError
      <$> o
      .: "error_code"
      <*> o
      .: "error_msg"
      <*> ( do
              arr :: Array <- o .: "request_params"
              V.forM arr $
                withObject "VKError/RequestParam" $
                  \rp -> (,) <$> rp .: "key" <*> rp .: "value"
          )

newtype WithResponse a = WithResponse a deriving (Show, Eq)

instance (FromJSON a) => FromJSON (WithResponse a) where
  parseJSON = withObject "WithResponse" $ \o -> do
    obj <- o .: "response"
    WithResponse <$> parseJSON obj

type AccessToken = T.Text
type Version = T.Text

mkClient :: AccessToken -> Version -> VKClient
mkClient a v = VKClient a v

mkDefaultClient :: AccessToken -> VKClient
mkDefaultClient a = mkClient a "5.131"

class ConvertibleToPart a where
  convertToPart :: a -> T.Text

instance ConvertibleToPart Int where
  convertToPart = T.pack . show

instance ConvertibleToPart Double where
  convertToPart = T.pack . show

instance ConvertibleToPart Bool where
  convertToPart True = "1"
  convertToPart False = "0"

instance ConvertibleToPart T.Text where
  convertToPart = id

instance ConvertibleToPart String where
  convertToPart = T.pack . show

instance (ConvertibleToPart a) => ConvertibleToPart (V.Vector a) where
  convertToPart = T.intercalate "," . V.toList . V.map convertToPart

instance ConvertibleToPart UTCTime where
  convertToPart = convertToPart . double2Int . realToFrac . utcTimeToPOSIXSeconds

mkPair :: (ConvertibleToPart a) => T.Text -> a -> (T.Text, T.Text)
mkPair lhs rhs = (lhs, convertToPart rhs)

(=--=) :: (ConvertibleToPart a) => T.Text -> a -> (T.Text, T.Text)
(=--=) = mkPair

sendMethod :: (FromJSON resp, MonadIO m) => VKClient -> T.Text -> [(T.Text, T.Text)] -> m (Either VKError resp)
sendMethod client methodName req = do
  rawResponse <-
    liftIO $
      postWith
        ( defaults
            & lensVL params
              .~ [ ("access_token", client.accessToken)
                 , ("v", client.version)
                 ]
        )
        (T.unpack $ "https://api.vk.com/method/" <> methodName)
        (map (uncurry partText) req)
  case rawResponse ^? lensVL responseBody % key "error" of
    Just err -> case fromJSON err of
      Error s -> error $ "unknown error: " <> s
      Success x -> pure $ Left x
    Nothing -> case decode (rawResponse ^. lensVL responseBody) of
      Nothing -> error "failed to decode response"
      Just x -> pure $ Right x

sendMethodUnsafe :: (FromJSON resp, MonadIO m) => VKClient -> T.Text -> [(T.Text, T.Text)] -> m resp
sendMethodUnsafe c m r = sendMethod c m r >>= \case
  Left err -> liftIO $ throwIO err
  Right x -> pure x
