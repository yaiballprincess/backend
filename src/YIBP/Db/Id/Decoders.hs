module YIBP.Db.Id.Decoders (idValue, idRow) where

import Hasql.Decoders
import YIBP.Core.Id

idValue :: Value (Id a)
idValue = fmap (Id . fromIntegral) int4

idRow :: Row (Id a)
idRow = column (nonNullable idValue)
