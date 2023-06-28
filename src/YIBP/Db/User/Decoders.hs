module YIBP.Db.User.Decoders (userRow) where

import YIBP.Db.User.Types

import Hasql.Decoders
import Control.Applicative

userRow :: Row RawUser
userRow = liftA3 RawUser
  (column (nonNullable text))
  (column (nonNullable bytea))
  (column (nonNullable bool))
