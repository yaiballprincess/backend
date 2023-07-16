module YIBP.Db.Rule.Types where

import YIBP.Core.Id
import YIBP.Core.Rule

data RawRule = RawRule
  { id :: !(Id Rule)
  , metadata :: !RuleMetadata
  , canTrigger :: !Bool
  , isActive :: !Bool
  }
