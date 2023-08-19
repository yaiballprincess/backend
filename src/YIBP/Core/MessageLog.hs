module YIBP.Core.MessageLog where

import Data.Text qualified as T
import Data.Time

data MessageLog = MessageLog
  { sentAt :: !UTCTime
  , result :: !MessageLogResult
  }
  deriving (Show, Eq)

type PollId = T.Text

data MessageLogResult
  = MessageLogOk !PollId
  | MessageLogError !T.Text
  deriving (Show, Eq)
