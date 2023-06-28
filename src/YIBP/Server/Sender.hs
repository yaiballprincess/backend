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
import YIBP.Db
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
  { _add :: route :- "add" :> ReqBody '[JSON] Sender :> Post '[JSON] Int
  , _all :: route :- "all" :> Get '[JSON] (V.Vector AllSenderUnitResponse)
  , _remove :: route :- "remove" :> ReqBody '[JSON] Int :> Delete '[JSON] NoContent
  , _edit :: route :- "edit" :> ReqBody '[JSON] EditSenderRequest :> Post '[JSON] NoContent
  }
  deriving (Generic)

theSenderAPI :: (WithDb) => SenderAPI (AsServerT Handler)
theSenderAPI =
  SenderAPI
    { _add = addHandler
    , _all = allHandler
    , _remove = removeHandler
    , _edit = editHandler
    }

addHandler :: (WithDb) => Sender -> Handler Int
addHandler req = do
  liftIO (insertSender req) >>= \case
    Just _id -> pure _id
    Nothing -> throwError err400

allHandler :: WithDb => Handler (V.Vector AllSenderUnitResponse)
allHandler = V.map (\(i, n) -> AllSenderUnitResponse i n) <$> liftIO getAllSenders

removeHandler :: (WithDb) => Int -> Handler NoContent
removeHandler _id = do
  isSuccess <- liftIO $ deleteSender _id
  if isSuccess then pure NoContent else throwError err400

editHandler :: (WithDb) => EditSenderRequest -> Handler NoContent
editHandler req = do
  isSuccess <-
    liftIO $ updateSender
      (req ^. #id)
      UpdateSenderPayload
        { newName = req ^. #newName
        , newAccessToken = req ^. #newAccessToken
        , newBotAccessToken = req ^. #newBotAccessToken
        }
  if isSuccess then pure NoContent else throwError err400
