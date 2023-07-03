module YIBP.Db.Id.Encoders (idParams) where

import YIBP.Core.Id

import Hasql.Encoders

import Data.Functor.Contravariant

idParams :: Params (Id a)
idParams = (\(Id x) -> fromIntegral x) >$< param (nonNullable int4)
