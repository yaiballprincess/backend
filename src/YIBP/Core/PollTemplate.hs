module YIBP.Core.PollTemplate
  ( PollTemplateOption (..)
  , getPollTemplateOption
  , PollTemplate (..)
  , CreatePollTemplate (..)
  , UpdatePollTemplate (..)
  , PollTemplateFull (..)
  ) where

import Data.Aeson
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import GHC.Generics
import YIBP.Core.Id

newtype PollTemplateOption = PollTemplateOption T.Text
  deriving newtype (Show, FromJSON, ToJSON)

getPollTemplateOption :: PollTemplateOption -> T.Text
getPollTemplateOption (PollTemplateOption t) = t

data PollTemplate = PollTemplate
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , question :: !T.Text
  , options :: !(V.Vector PollTemplateOption)
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CreatePollTemplate = CreatePollTemplate
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , question :: !T.Text
  }
  deriving (Generic, FromJSON, ToJSON)

data UpdatePollTemplate = UpdatePollTemplate
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , question :: !T.Text
  }
  deriving (Generic, FromJSON, ToJSON)

data PollTemplateFull = PollTemplateFull
  { id :: !(Id PollTemplate)
  , isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , question :: !T.Text
  , options :: !(V.Vector (IdObject PollTemplateOption))
  }
  deriving (Show, Generic, ToJSON)
