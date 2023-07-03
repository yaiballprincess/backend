module YIBP.Db.Id.Encoders (idValue, idParams) where

import YIBP.Core.Id

import Hasql.Encoders

import Data.Functor.Contravariant

idValue :: Value (Id a)
idValue = contramap (\(Id x) -> fromIntegral x) int4

idParams :: Params (Id a)
idParams = param (nonNullable idValue)
