module YIBP.Db.Session.Decoders (refreshTokenRow, sessionRow) where

import Hasql.Decoders

import YIBP.Core.Session
import YIBP.Db.Id.Decoders

refreshTokenRow :: Row RefreshToken
refreshTokenRow = RefreshToken <$> column (nonNullable uuid)

sessionRow :: Row Session
sessionRow =
  Session
    <$> refreshTokenRow
    <*> idRow
    <*> column (nonNullable timestamptz)
