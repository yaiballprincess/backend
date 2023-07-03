module YIBP.Core.Rule where

import Data.Time
import GHC.Generics
import YIBP.Scheduler.Util

data RegularRule = RegularRule
  { receiverId :: !Int
  , pollTemplateId :: !Int
  , cronRule :: !MyCronSchedule
  }
  deriving (Show, Eq, Generic)

data ExceptionRule = ExceptionRule
  { regularRuleId :: !Int
  , receiverId :: !Int
  , pollTemplateId :: !Int
  , sendAt :: !UTCTime
  }
  deriving (Show, Eq, Generic)
