{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.Sender (SenderAPI, theSenderAPI) where

import Servant
import Servant.API.Generic
import Servant.Server.Generic

import Data.Vector qualified as V

import YIBP.Core.Id
import YIBP.Core.Receiver
import YIBP.Core.Sender
import YIBP.Db
import YIBP.Service.Receiver qualified as Service
import YIBP.Service.Sender qualified as Service

import Control.Monad.Catch (catch)
import Control.Monad.IO.Class
import YIBP.Error

data SenderAPI route = SenderAPI
  { _addSender
      :: route
        :- ReqBody '[JSON] CreateSenderParam
        :> Post '[JSON] (Id SenderTag)
  , _removeSender
      :: route
        :- Capture "id" (Id SenderTag)
        :> Delete '[JSON] NoContent
  , _getSenders
      :: route
        :- Get '[JSON] (V.Vector SenderTrimmed)
  , _addReceiver
      :: route
        :- Capture "id" (Id SenderTag)
        :> ReqBody '[JSON] CreateReceiverParam
        :> Post '[JSON] NoContent
  , _getReceivers
      :: route
        :- Capture "id" (Id SenderTag)
        :> Get '[JSON] (V.Vector Receiver)
  , _removeReceiver
      :: route
        :- Capture "id" (Id SenderTag)
        :> Capture "peer-id" PeerIdParam
        :> Delete '[JSON] NoContent
  }
  deriving (Generic)

theSenderAPI :: (WithDb) => SenderAPI (AsServerT Handler)
theSenderAPI =
  SenderAPI
    { _addSender = addSenderHandler
    , _removeSender = removeSenderHandler
    , _getSenders = getSendersHandler
    , _addReceiver = addReceiverHandler
    , _getReceivers = getReceiversHandler
    , _removeReceiver = removeReceiverHandler
    }

addSenderHandler :: (WithDb) => CreateSenderParam -> Handler (Id SenderTag)
addSenderHandler csp =
  liftIO (Service.createSender csp)
    `catch` (\(Service.VKUserError e) -> raiseServantError (HttpErrorWithDetails @Service.VKUserError "VK API user error: maybe user token is invalid" e) err422)
    `catch` (\(Service.VKGroupError e) -> raiseServantError (HttpErrorWithDetails @Service.VKGroupError "VK API group error: maybe group token is invalid" e) err422)
    `catch` (\Service.SenderConflict -> raiseServantError (HttpError @Service.SenderConflict "Such sender already exists") err409)

removeSenderHandler :: (WithDb) => Id SenderTag -> Handler NoContent
removeSenderHandler senderId = do
  liftIO (Service.removeSender senderId)
    `catch` (\Service.SenderDoesNotExist -> raiseServantError (HttpError @Service.SenderDoesNotExist "Sender with such id does not exist") err404)
  pure NoContent

getSendersHandler :: (WithDb) => Handler (V.Vector SenderTrimmed)
getSendersHandler = liftIO Service.getSendersTrimmed

addReceiverHandler :: (WithDb) => Id SenderTag -> CreateReceiverParam -> Handler NoContent
addReceiverHandler senderId crp = do
  liftIO (Service.createReceiver senderId crp)
    `catch` (\Service.SenderDoesNotExist -> raiseServantError (HttpError @Service.SenderDoesNotExist "Sender with such id does not exist") err404)
    `catch` (\Service.ReceiverConflict -> raiseServantError (HttpError @Service.ReceiverConflict "Receiver conflict") err409)
  pure NoContent

getReceiversHandler :: (WithDb) => Id SenderTag -> Handler (V.Vector Receiver)
getReceiversHandler senderId =
  liftIO (Service.getReceivers senderId)
    `catch` (\Service.SenderDoesNotExist -> raiseServantError (HttpError @Service.SenderDoesNotExist "Sender with such id does not exist") err404)

removeReceiverHandler :: (WithDb) => Id SenderTag -> PeerIdParam -> Handler NoContent
removeReceiverHandler senderId (PeerIdParam peerId) = do
  liftIO (Service.removeReceiver senderId peerId)
    `catch` (\Service.ReceiverDoesNotExist -> raiseServantError (HttpError @Service.ReceiverDoesNotExist "Receiver does not exist") err404)
  pure NoContent
