module YIBP.Db.Rule.Encoders where

import Data.Aeson
import Data.Functor.Contravariant

import Hasql.Encoders
import YIBP.Core.Rule

ruleParams :: Params Rule
ruleParams =
  ((toJSON . (.metadata)) >$< param (nonNullable jsonb))
    <> ((.isActive) >$< param (nonNullable bool))
