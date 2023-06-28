{-# LANGUAGE OverloadedStrings #-}

module YIBP.Error where

import Control.Monad.Except
import Data.Aeson
import Data.Kind
import Data.Text qualified as T
import Data.Typeable
import Servant

type HttpError :: Type -> Maybe Type -> Type
data family HttpError err details

data instance HttpError err ('Just details) = HttpErrorWithDetails !T.Text !details
newtype instance HttpError err 'Nothing = HttpError T.Text

instance (Typeable err, ToJSON details) => ToJSON (HttpError err ('Just details)) where
  toJSON (HttpErrorWithDetails msg details) =
    object
      [ "error" .= errStr
      , "message" .= msg
      , "details" .= toJSON details
      ]
    where
      errStr = show (typeRep (Proxy :: Proxy err))

instance (Typeable err) => ToJSON (HttpError err 'Nothing) where
  toJSON (HttpError msg) =
    object
      [ "error" .= errStr
      , "message" .= msg
      ]
    where
      errStr = show (typeRep (Proxy :: Proxy err))

transformServantError
  :: (ToJSON (HttpError err details))
  => HttpError err details
  -> ServerError
  -> ServerError
transformServantError e se =
  se
    { errHeaders = [("Content-Type", "application/json")]
    , errBody = encode e
    }

raiseServantError
  :: (MonadError ServerError m, ToJSON (HttpError err details))
  => HttpError err details
  -> ServerError
  -> m a
raiseServantError e se = throwError $ transformServantError e se
