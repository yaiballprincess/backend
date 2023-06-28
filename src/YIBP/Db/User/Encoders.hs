module YIBP.Db.User.Encoders (insertUserParams) where

import YIBP.Db.User.Types

import Hasql.Encoders

import Data.Functor.Contravariant

insertUserParams :: Params InsertUser
insertUserParams =
  ((\u -> u.username) >$< param (nonNullable text))
    <> ((\u -> u.hashedPassword) >$< param (nonNullable bytea))
    <> ((\u -> u.isAdmin) >$< param (nonNullable bool))