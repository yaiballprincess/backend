module YIBP.Db.MessageLog.Types where

import YIBP.Core.Id
import YIBP.Core.MessageLog

data RawMessageLog = RawMessageLog
  { messageLogId :: !(Id MessageLog)
  , messsageLog :: !MessageLog
  }
  deriving (Show, Eq)
