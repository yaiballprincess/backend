module YIBP.Db.User.Decoders (userRow) where

import YIBP.Db.User.Types

import Hasql.Decoders

userRow :: Row RawUser
userRow =
  RawUser
    <$> fmap fromIntegral (column (nonNullable int4))
    <*> column (nonNullable text)
    <*> column (nonNullable bytea)
    <*> column (nonNullable bool)
