{-# LANGUAGE DerivingVia #-}

module YIBP.Core.Rule where

import Data.Aeson
import Data.Text qualified as T
import Data.Time (UTCTime)

import YIBP.Scheduler.Util

import YIBP.Core.Id
import YIBP.Core.PollTemplate
import YIBP.Core.Receiver
import YIBP.Core.Sender

import Deriving.Aeson

data Rule = Rule
  { metadata :: !RuleMetadata
  , isActive :: !Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data RuleMetadata
  = RMRegular !RegularRule
  | RMIgnore !IgnoreRule
  | RMReplace !ReplaceRule
  deriving (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[ SumObjectWithSingleField
           , ConstructorTagModifier '[StripPrefix "RM", CamelToSnake]
           ]
          RuleMetadata

data RegularRule = RegularRule
  { name :: !T.Text
  , schedule :: !MyCronSchedule
  , senderId :: !(Id SenderTag)
  , peerId :: !Int
  , pollTemplateId :: !(Id PollTemplate)
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data IgnoreRule = IgnoreRule
  { regularRuleId :: !(Id Rule)
  , startsAt :: !UTCTime
  , endsAt :: !UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ReplaceRule = ReplaceRule
  { regularRuleId :: !(Id Rule)
  , startsAt :: !UTCTime
  , endsAt :: !UTCTime
  , newPollTemplateId :: !(Id PollTemplate)
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data RegularRuleFull = RegularRuleFull
  { name :: !T.Text
  , schedule :: !MyCronSchedule
  , receiver :: !Receiver
  , sender :: !SenderTrimmed
  , pollTemplate :: !(IdObject PollTemplateFull)
  }
  deriving (Generic, ToJSON)

data ReplaceRuleFull = ReplaceRuleFull
  { regularRuleId :: !(Id Rule)
  , startsAt :: !UTCTime
  , endsAt :: !UTCTime
  , newPollTemplate :: !(IdObject PollTemplateFull)
  }
  deriving (Generic, ToJSON)

data RuleMetadataFull
  = RMFRegular !RegularRuleFull
  | RMFIgnore !IgnoreRule
  | RMFReplace !ReplaceRuleFull
  deriving (Generic)
  deriving
    (ToJSON)
    via CustomJSON
          '[ SumObjectWithSingleField
           , ConstructorTagModifier '[StripPrefix "RMF", CamelToSnake]
           ]
          RuleMetadataFull

data RuleFull = RuleFull
  { id :: !(Id Rule)
  , metadata :: !RuleMetadataFull
  , isActive :: !Bool
  }
  deriving (Generic, ToJSON)
