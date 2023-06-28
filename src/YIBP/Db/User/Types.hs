module YIBP.Db.User.Types (InsertUser (..), RawUser (..)) where

import Data.ByteString qualified as BS
import Data.Text qualified as T

data InsertUser = InsertUser
  { username :: !T.Text
  , hashedPassword :: !BS.ByteString
  , isAdmin :: !Bool
  }

data RawUser = RawUser
  { id :: !Int
  , username :: !T.Text
  , hashedPassword :: !BS.ByteString
  , isAdmin :: !Bool
  }
