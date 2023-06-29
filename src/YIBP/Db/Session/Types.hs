module YIBP.Db.Session.Types (InsertSession (..), UpdateSession (..)) where

import Data.Time
import YIBP.Core.Id
import YIBP.Core.Session
import YIBP.Core.User

data InsertSession = InsertSession
  { refreshToken :: !RefreshToken
  , ownerId :: !(Id User)
  , createdAt :: !UTCTime
  }

data UpdateSession = UpdateSession
  { oldRefreshToken :: !RefreshToken
  , newRefreshToken :: !RefreshToken
  , createdAt :: !UTCTime
  }
