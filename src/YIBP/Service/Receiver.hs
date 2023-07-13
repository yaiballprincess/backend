module YIBP.Service.Receiver where

import YIBP.Core.Id
import YIBP.Core.Receiver
import YIBP.Core.Sender

import YIBP.Service.Sender
import YIBP.Service.VK
import YIBP.VK.Client

import YIBP.Db
import YIBP.Db.Receiver
import YIBP.Db.Receiver.Types
import YIBP.Db.Sender
import YIBP.Db.Sender.Types

import Control.Exception (Exception, throwIO)

data ReceiverConflict = ReceiverConflict
  deriving (Show, Eq, Exception)

data ReceiverDoesNotExist = ReceiverDoesNotExist
  deriving (Show, Eq, Exception)

createReceiver :: (WithDb) => Id SenderTag -> CreateReceiverParam -> IO ()
createReceiver senderId crp = do
  rawSender <-
    getSenderById senderId >>= \case
      Just v -> pure v
      Nothing -> throwIO SenderDoesNotExist
  let encryptedSender =
        Sender
          { name = rawSender.name
          , accessToken = rawSender.accessToken
          , bot = fmap (\raw -> BotSender {accessToken = raw.accessToken, id = raw.id}) rawSender.bot
          }
  let sender = decryptSender encryptedSender
  name <- getNameByPeerId (mkDefaultClient sender.accessToken) ((.id) <$> sender.bot) crp.peerId
  let ir = InsertReceiver {senderId = senderId, name = name, peerId = crp.peerId}
  insertReceiver ir >>= \case
    True -> pure ()
    False -> throwIO ReceiverConflict

removeReceiver :: WithDb => Id SenderTag -> Int -> IO ()
removeReceiver senderId peerId = deleteReceiver senderId peerId >>= \case
  True -> pure ()
  False -> throwIO ReceiverDoesNotExist
