{-# LANGUAGE OverloadedStrings #-}

module YIBP.Service.VK where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V

import YIBP.VK.Client qualified as VK
import YIBP.VK.Types

import YIBP.Core.Sender
import YIBP.Crypto

getNameByPeerId :: Sender 'Decrypted -> Int -> IO T.Text
getNameByPeerId sender peerId
  | peerId > 2_000_000_000 = do
      VK.WithResponse response <- VK.sendMethodUnsafe @(VK.WithResponse (VKArray VKConversation)) client "messages.getConversationsById" [VK.mkPair "peer_ids" peerId]
      let conversation = V.head response.items
      pure $ case conversation.chatSettings of
        Just chat -> chat.title
        Nothing -> ""
  | peerId > 0 = do
      VK.WithResponse users <- VK.sendMethodUnsafe @(VK.WithResponse (V.Vector VKUserFull)) client "users.get" [VK.mkPair "user_ids" peerId]
      let user = V.head users
      let firstName = fromMaybe "" user.firstName
      let lastName = fromMaybe "" user.lastName
      pure $ firstName <> T.pack " " <> lastName
  | otherwise = do
      VK.WithResponse groups <- VK.sendMethodUnsafe @(VK.WithResponse (V.Vector VKGroupFull)) client "groups.getById" [VK.mkPair "group_ids" (-peerId)]
      let group = V.head groups
      pure $ fromMaybe "" group.name
  where
    client = VK.mkDefaultClient (getSenderToken sender)
