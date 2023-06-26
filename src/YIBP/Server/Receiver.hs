{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.Receiver (ReceiverAPI (..), theReceiverAPI) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)

import Data.Aeson
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson
import Deriving.Aeson.Stock

import Servant
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Server
import Servant.Server.Generic

import YIBP.Core.Receiver
import YIBP.Core.Sender
import YIBP.Util.WithId

import Data.Maybe (fromMaybe)
import Data.Vector.Optics
import Optics
import YIBP.Db.Receiver
import YIBP.Db.Sender (getSenderById)
import YIBP.Db.Db
import YIBP.VK.Client
import YIBP.VK.Types

data AddRequest = AddRequest
  { senderId :: !Int
  , receiver :: !Receiver
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type ReceiverId = Int
type SenderId = Int

makeFieldsNoPrefix ''AddRequest

data ReceiverAPI route = ReceiverAPI
  { _add :: route :- "add" :> ReqBody '[JSON] AddRequest :> Post '[JSON] ReceiverId
  , _getConversations :: route :- "get-conversations" :> ReqBody '[JSON] SenderId :> Get '[JSON] (V.Vector Receiver)
  , _remove :: route :- "remove" :> ReqBody '[JSON] ReceiverId :> Delete '[JSON] NoContent
  , _getAll :: route :- "get-all" :> Get '[JSON] (V.Vector (WithId Receiver))
  }
  deriving (Generic)

theReceiverAPI :: WithDb => ReceiverAPI (AsServerT Handler)
theReceiverAPI =
  ReceiverAPI
    { _add = addHandler
    , _getConversations = getConversationsHandler
    , _remove = removeHandler
    , _getAll = getAllHandler
    }

addHandler :: WithDb => AddRequest -> Handler ReceiverId
addHandler req = do
  liftIO (insertReceiver (req ^. #senderId) (req ^. #receiver)) >>= \case
    Just _id -> pure _id
    Nothing -> throwError err422

convert :: VKMessagesGetConversationsResponse -> V.Vector Receiver
convert resp = f =<< toVectorOf (#items % traversed % #conversation) resp
  where
    f :: VKConversation -> V.Vector Receiver
    f conversation = case conversation ^. #peer % #_type of
      VKConversationPeerChat -> case conversation ^. #chatSettings of
        Nothing -> V.empty
        Just settings ->
          V.singleton $
            Receiver
              { name = settings ^. #title
              , peerId = peerId
              }
      VKConversationPeerEmail -> V.empty
      VKConversationPeerUser -> maybe V.empty V.singleton (findUserById peerId)
      VKConversationPeerGroup -> maybe V.empty V.singleton (findGroupById (-peerId))
      where
        peerId = conversation ^. #peer % #_id

    findUserById :: Int -> Maybe Receiver
    findUserById _id = r <$> V.find (\x -> x ^. #_id == _id) (fromMaybe V.empty (resp ^. #profiles))
      where
        r :: VKUserFull -> Receiver
        r p =
          Receiver
            { name = fromMaybe "" (p ^. #_firstName) <> " " <> fromMaybe "" (p ^. #_lastName)
            , peerId = _id
            }

    findGroupById :: Int -> Maybe Receiver
    findGroupById _id = r <$> V.find (\x -> x ^. #_id == _id) (fromMaybe V.empty (resp ^. #groups))
      where
        r :: VKGroupFull -> Receiver
        r p =
          Receiver
            { name = fromMaybe "" (p ^. #_name)
            , peerId = -_id
            }

getConversationsHandler :: (WithDb) => SenderId -> Handler (V.Vector Receiver)
getConversationsHandler req = do
  mSender <- liftIO $ getSenderById req -- TODO: use lambda case
  case mSender of
    Nothing -> throwError err422
    Just sender -> do
      let token = fromMaybe (sender ^. #accessToken) (sender ^. #botAccessToken)
      let vkClient = mkDefaultClient token
      resp <-
        sendMethod @(WithResponse VKMessagesGetConversationsResponse)
          vkClient
          "messages.getConversations"
          [ mkPair "count" ("200" :: T.Text)
          , mkPair "extended" True
          ]
      case resp of
        Left _ -> throwError err422 -- TODO: proper error handling
        Right (WithResponse response) -> do
          pure (convert response)

removeHandler :: (WithDb) => ReceiverId -> Handler NoContent
removeHandler req = do
  liftIO (deleteReceiver req) >>= \case
    True -> pure NoContent
    False -> throwError err422

getAllHandler :: (WithDb) => Handler (V.Vector (WithId Receiver))
getAllHandler = do
  V.map (\(idx, recv) -> WithId idx recv) <$> liftIO getAllReceivers