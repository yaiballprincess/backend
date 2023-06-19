{-# LANGUAGE DeriveAnyClass #-}

module YIBP.Server.Sender (SenderAPI, theSenderAPI) where

import Servant
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Servant.Server.Generic

import Data.Aeson
import Data.Text qualified as T
import Data.Vector qualified as V

import Control.Monad.Except
import Control.Monad.IO.Class

import YIBP.App
import YIBP.Core.Sender
import YIBP.Db.Sender
import YIBP.Db.Util
import YIBP.Server.Auth (AuthData, withAuth)

import Optics

data EditSenderRequest = EditSenderRequest
  { id :: !Int
  , newName :: !(Maybe T.Text)
  , newAccessToken :: !(Maybe T.Text)
  , newBotAccessToken :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeFieldsNoPrefix ''EditSenderRequest

data AllSenderUnitResponse = AllSenderUnitResponse
  { id :: !Int
  , name :: !T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeFieldsNoPrefix ''AllSenderUnitResponse

data SenderAPI route = SenderAPI
  { _add :: route :- Auth '[JWT] AuthData :> "add" :> ReqBody '[JSON] Sender :> Post '[JSON] NoContent
  , _all :: route :- Auth '[JWT] AuthData :> "all" :> Get '[JSON] (V.Vector AllSenderUnitResponse)
  , _remove :: route :- Auth '[JWT] AuthData :> "remove" :> ReqBody '[JSON] Int :> Delete '[JSON] NoContent
  , _edit :: route :- Auth '[JWT] AuthData :> "edit" :> ReqBody '[JSON] EditSenderRequest :> Post '[JSON] NoContent
  }
  deriving (Generic)

theSenderAPI :: (MonadIO m, MonadError ServerError m) => SenderAPI (AsServerT (AppT m))
theSenderAPI =
  SenderAPI
    { _add = addHandler
    , _all = allHandler
    , _remove = removeHandler
    , _edit = editHandler
    }

addHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => AuthResult AuthData -> Sender -> m NoContent
addHandler _authData req = withAuth _authData $ do
  _id <- insertSender req
  case _id of
    Just _ -> pure NoContent
    Nothing -> throwError err400

allHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => AuthResult AuthData -> m (V.Vector AllSenderUnitResponse)
allHandler _authData = withAuth _authData $ do
  V.map (\(i, n) -> AllSenderUnitResponse i n) <$> getAllSenders

removeHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => AuthResult AuthData -> Int -> m NoContent
removeHandler _authData _id = withAuth _authData $ do
  isSuccess <- deleteSender _id
  if isSuccess then pure NoContent else throwError err400

editHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => AuthResult AuthData -> EditSenderRequest -> m NoContent
editHandler _authData req = withAuth _authData $ do
  isSuccess <-
    updateSender
      (req ^. #id)
      UpdateSenderPayload
        { newName = req ^. #newName
        , newAccessToken = req ^. #newAccessToken
        , newBotAccessToken = req ^. #newBotAccessToken
        }
  if isSuccess then pure NoContent else throwError err400