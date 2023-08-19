{-# LANGUAGE OverloadedStrings #-}

module YIBP.Service.Sender where

import Data.Text qualified as T
import Data.Vector qualified as V

import YIBP.Core.Sender
import YIBP.Crypto

import YIBP.Db (WithDb)
import YIBP.Db.Sender qualified as Db
import YIBP.Db.Sender.Types

import YIBP.Core.Id (Id, idToInt)

import Control.Exception (Exception, throwIO)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Functor ((<&>))
import Data.Maybe
import Fmt
import YIBP.Config
import YIBP.VK.Client qualified as VK
import YIBP.VK.Types

decryptBotSender :: WithConfig => BotSender 'Encrypted -> BotSender 'Decrypted
decryptBotSender bot =
  BotSender
    { accessToken = decryptCryptoText bot.accessToken
    , id = bot.id
    }

encryptBotSender :: WithConfig => BotSender 'Decrypted -> IO (BotSender 'Encrypted)
encryptBotSender bot = do
  encryptedAccessToken <- encryptCryptoTextRandom bot.accessToken
  pure $ BotSender {accessToken = encryptedAccessToken, id = bot.id}

decryptSender :: WithConfig => Sender 'Encrypted -> Sender 'Decrypted
decryptSender sender =
  Sender
    { name = sender.name
    , accessToken = decryptCryptoText sender.accessToken
    , bot = decryptBotSender <$> sender.bot
    }

encryptSender :: WithConfig => Sender 'Decrypted -> IO (Sender 'Encrypted)
encryptSender sender = do
  encryptedAccessToken <- encryptCryptoTextRandom sender.accessToken
  encryptedBot <- case sender.bot of
    Nothing -> pure Nothing
    Just x -> Just <$> encryptBotSender x
  pure $
    Sender
      { name = sender.name
      , accessToken = encryptedAccessToken
      , bot = encryptedBot
      }

newtype VKUserError = VKUserError Int
  deriving (Show, Eq)
  deriving anyclass (Exception)

newtype VKGroupError = VKGroupError Int
  deriving (Show, Eq)
  deriving anyclass (Exception)

data SenderConflict = SenderConflict
  deriving (Show, Eq, Exception)

createSender :: (WithDb, WithConfig) => CreateSenderParam -> IO (Id SenderTag)
createSender csp = do
  let userClient = VK.mkDefaultClient csp.accessToken
  userName <-
    VK.sendMethod @(VK.WithResponse (V.Vector VKUserFull)) userClient "users.get" [] >>= \case
      Left err -> throwIO $ VKUserError err.errorCode
      Right (VK.WithResponse users) -> do
        let user = V.head users
        let firstName = fromMaybe "" user.firstName
        let lastName = fromMaybe "" user.lastName
        pure $ firstName <> T.pack " " <> lastName
  encryptedAccessToken <- encryptCryptoTextRandom csp.accessToken
  bot <- runMaybeT $ do
    botAccessToken <- MaybeT $ pure $ csp.botAccessToken
    let botClient = VK.mkDefaultClient botAccessToken
    liftIO $ do
      VK.sendMethod @(VK.WithResponse (V.Vector VKGroupFull)) botClient "groups.getById" [] >>= \case
        Left err -> throwIO $ VKGroupError err.errorCode
        Right (VK.WithResponse groups) -> do
          unless (V.length groups == 1) $ do
            throwIO $ VKGroupError 0
          let group = V.head groups
          encryptedBotAccessToken <- encryptCryptoTextRandom botAccessToken
          pure $ BotSender {accessToken = encryptedBotAccessToken, id = group.id}
  Db.insertSender
    InsertSender
      { name = userName
      , accessToken = encryptedAccessToken
      , bot = bot
      }
    >>= \case
      Just v -> pure v
      Nothing -> throwIO SenderConflict

data SenderDoesNotExist = SenderDoesNotExist
  deriving (Show, Eq, Exception)

removeSender :: (WithDb) => Id SenderTag -> IO ()
removeSender senderId = do
  Db.deleteSender senderId >>= \case
    True -> pure ()
    False -> throwIO SenderDoesNotExist

getSendersTrimmed :: (WithDb) => IO (V.Vector SenderTrimmed)
getSendersTrimmed =
  V.map
    ( \s ->
        SenderTrimmed
          { id = idToInt s.id
          , name = s.name
          , botName = (\i -> "vk.com/club" +| i |+ "") <$> s.botId
          }
    )
    <$> Db.getSendersTrimmed

getSenderById :: (WithDb) => Id SenderTag -> IO (Maybe (Sender 'Encrypted))
getSenderById sId = fmap tr <$> Db.getSenderById sId
  where
    tr r =
      Sender
        { name = r.name
        , accessToken = r.accessToken
        , bot = BotSender <$> ((.accessToken) <$> r.bot) <*> ((.id) <$> r.bot)
        }
