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
  , VKPoll (..)
  , VKArray (..)
  ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson
  ( CamelToSnake
  , CustomJSON (CustomJSON)
  , FieldLabelModifier
  , Generic
  , OmitNothingFields
  , StripPrefix
  )

data VKUserFull = VKUserFull
  { firstName :: !(Maybe T.Text)
  , lastName :: !(Maybe T.Text)
  , id :: !Int
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKUserFull

data VKGroupFull = VKGroupFull
  { id :: !Int
  , name :: !(Maybe T.Text)
  , screenName :: !(Maybe T.Text)
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKGroupFull

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
  { id :: !Int
  , _type :: !VKConversationPeerType
  , localId :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "_", CamelToSnake]] VKConversationPeer

data VKChatSettings = VKChatSettings
  { ownerId :: !Int
  , title :: !T.Text
  , membersCount :: !(Maybe Int)
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKChatSettings

data VKConversation = VKConversation
  { peer :: !VKConversationPeer
  , lastMessageId :: !Int
  , chatSettings :: !(Maybe VKChatSettings)
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKConversation

data VKConversationWithMessage = VKConversationWithMessage
  { conversation :: !VKConversation
  , lastMessage :: !()
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKConversationWithMessage

data VKMessagesGetConversationsResponse = VKMessagesGetConversationsResponse
  { items :: !(V.Vector VKConversationWithMessage)
  , profiles :: !(Maybe (V.Vector VKUserFull))
  , groups :: !(Maybe (V.Vector VKGroupFull))
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToSnake]] VKMessagesGetConversationsResponse

data VKPoll = VKPoll
  { id :: !Int
  , ownerId :: !Int
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] VKPoll

data VKArray a = VKArray
  { count :: !Int
  , items :: !(V.Vector a)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
