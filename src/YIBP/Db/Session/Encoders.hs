module YIBP.Db.Session.Encoders (refreshTokenParams, insertSessionParams, updateSessionParams) where

import YIBP.Core.Session

import Data.Functor.Contravariant
import Hasql.Encoders
import YIBP.Db.Id.Encoders (idParams)
import YIBP.Db.Session.Types

refreshTokenParams :: Params RefreshToken
refreshTokenParams = (\(RefreshToken t) -> t) >$< param (nonNullable uuid)

insertSessionParams :: Params InsertSession
insertSessionParams =
  ((\is -> is.refreshToken) >$< refreshTokenParams)
    <> ((\is -> is.ownerId) >$< idParams)
    <> ((\is -> is.createdAt) >$< param (nonNullable timestamptz))

updateSessionParams :: Params UpdateSession
updateSessionParams =
  ((\us -> us.oldRefreshToken) >$< refreshTokenParams)
    <> ((\us -> us.newRefreshToken) >$< refreshTokenParams)
    <> ((\us -> us.createdAt) >$< (param (nonNullable timestamptz)))
