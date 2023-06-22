{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module YIBP.VK.Types
  ( VKConversationPeerType (..)
  , VKConversationPeer (..)
  , VKChatSettings (..)
  , VKConversation (..)
  , VKUserFull (..)
  , VKGroupFull (..)
  , VKConversationWithMessage (..)
  , VKMessagesGetConversationsResponse (..)
  ) where

import Data.Aeson
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson
import Deriving.Aeson.Stock

import Optics

data VKUserFull = VKUserFull
  { _firstName :: !(Maybe T.Text)
  , _lastName :: !(Maybe T.Text)
  , _id  :: !Int
  } deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_", CamelToSnake]] VKUserFull

makeFields ''VKUserFull

data VKGroupFull = VKGroupFull
  { _id :: !Int
  , _name :: !(Maybe T.Text)
  , _screenName :: !(Maybe T.Text)
  } deriving (Eq, Show, Generic)
  deriving (FromJSON, ToJSON)
  via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_", CamelToSnake]] VKGroupFull

data VKConversationPeerType
  = VKConversationPeerChat
  | VKConversationPeerEmail
  | VKConversationPeerUser
  | VKConversationPeerGroup
  deriving (Show, Eq)

instance FromJSON VKConversationPeerType where
  parseJSON = withText "VKConversationPeerType" $ \case
    "chat" -> pure VKConversationPeerChat
    "email" -> pure VKConversationPeerEmail
    "user" -> pure VKConversationPeerUser
    "group" -> pure VKConversationPeerGroup
    _ -> fail "unknown type"

instance ToJSON VKConversationPeerType where
  toJSON VKConversationPeerChat = "chat"
  toJSON VKConversationPeerEmail = "email"
  toJSON VKConversationPeerUser = "user"
  toJSON VKConversationPeerGroup = "group"

data VKConversationPeer = VKConversationPeer
  { _id :: !Int
  , _type :: !VKConversationPeerType
  , _localId :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_", CamelToSnake]] VKConversationPeer

makeFields ''VKConversationPeer

data VKChatSettings = VKChatSettings
  { ownerId :: !Int
  , title :: !T.Text
  , membersCount :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKChatSettings

makeFieldsNoPrefix ''VKChatSettings

data VKConversation = VKConversation
  { peer :: !VKConversationPeer
  , lastMessageId :: !Int
  , chatSettings :: !(Maybe VKChatSettings)
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKConversation

makeFieldsNoPrefix ''VKConversation

data VKConversationWithMessage = VKConversationWithMessage
  { conversation :: !VKConversation
  , lastMessage :: !()
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKConversationWithMessage

makeFieldsNoPrefix ''VKConversationWithMessage

data VKMessagesGetConversationsResponse = VKMessagesGetConversationsResponse
  { items :: !(V.Vector VKConversationWithMessage)
  , profiles :: !(Maybe (V.Vector VKUserFull))
  , groups :: !(Maybe (V.Vector VKGroupFull))
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKMessagesGetConversationsResponse

makeFieldsNoPrefix ''VKMessagesGetConversationsResponse

