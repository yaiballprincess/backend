module YIBP.Db.PollTemplate.Types where

import Data.Text qualified as T
import Data.Time
import YIBP.Core.Id
import YIBP.Core.PollTemplate qualified as Core

data InsertPollTemplateParams = InsertPollTemplateParams
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , question :: !T.Text
  }

data UpdatePollTemplateParams = UpdatePollTemplateParams
  { id :: !(Id Core.PollTemplate)
  , isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , question :: !T.Text
  }
