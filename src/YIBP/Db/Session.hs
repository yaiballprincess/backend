{-# LANGUAGE OverloadedStrings #-}

module YIBP.Db.Session (insertRefreshToken, updateRefreshTokenWithValidationCheck) where

import YIBP.Core.Session

import Contravariant.Extras
import Data.Time
import GHC.Stack
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Session qualified as Session
import Hasql.Statement
import YIBP.Db
import YIBP.Db.Session.Decoders
import YIBP.Db.Session.Encoders
import YIBP.Db.Session.Types

insertRefreshToken :: (WithDb, HasCallStack) => InsertSession -> IO ()
insertRefreshToken is = withConn (Session.run (Session.statement is stmt))
  where
    stmt =
      Statement
        "insert into \"session\" (id, owner_id, created_at) values ($1, $2, $3)"
        insertSessionParams
        Decoders.noResult
        True

updateRefreshTokenWithValidationCheck
  :: (WithDb, HasCallStack)
  => DiffTime
  -> RefreshToken
  -> IO (Maybe Session)
updateRefreshTokenWithValidationCheck refreshTokenDuration refreshToken =
  withConn (Session.run (Session.statement (refreshTokenDuration, refreshToken) stmt))
  where
    stmt =
      Statement
        "update \"session\" \
        \ set id = gen_random_uuid(), created_at = now() \
        \ where id = $2 and (now() - created_at < $1) \
        \ returning id, owner_id, created_at"
        ( contrazip2
            (Encoders.param (Encoders.nonNullable Encoders.interval))
            refreshTokenParams
        )
        (Decoders.rowMaybe sessionRow)
        True
