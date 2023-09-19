module YIBP.Scheduler.Internal where

import YIBP.Core.Id qualified as Core
import YIBP.Core.Rule qualified as Core

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import Data.Time (fromGregorian)
import Data.Time.Clock

import Optics

data Rule = Rule
  { _regular :: !Core.RegularRule
  , _ignoreRules :: !(IntMap Core.IgnoreRule)
  , _replaceRules :: !(IntMap Core.ReplaceRule)
  }
  deriving (Show)

makeFieldsNoPrefix ''Rule

data State = State
  { _rules :: !(IntMap Rule)
  , _nextTime :: !(IntMap UTCTime)
  }
  deriving (Show)

makeFieldsNoPrefix ''State

type RuleId = Core.Id Core.Rule

addRegularRule :: RuleId -> Core.RegularRule -> State -> State
addRegularRule rId r =
  (rules %!~ (at idInt ?~ Rule r mempty mempty))
    . (nextTime %!~ (at idInt ?~ maxDate))
  where
    idInt = Core.idToInt rId
    maxDate =
      addUTCTime
        (fromIntegral (maxBound :: Int))
        (UTCTime (fromGregorian 0 1 1) 0)

addReplaceRule :: RuleId -> Core.ReplaceRule -> State -> State
addReplaceRule rId r =
  rules
    %~ ( at regularRuleId
          %~ fmap (replaceRules %~ (at replaceRuleId ?!~ r))
       )
  where
    regularRuleId = Core.idToInt r.regularRuleId
    replaceRuleId = Core.idToInt rId

addIgnoreRule :: RuleId -> Core.IgnoreRule -> State -> State
addIgnoreRule rId r =
  rules
    %~ ( at regularRuleId
          %~ fmap (ignoreRules %~ (at ignoreRuleId ?!~ r))
       )
  where
    regularRuleId = Core.idToInt r.regularRuleId
    ignoreRuleId = Core.idToInt rId

deleteRegularRule :: RuleId -> State -> State
deleteRegularRule rId =
  (rules %!~ (at idInt %~ const Nothing))
    . (nextTime %!~ (at idInt %~ const Nothing))
  where
    idInt = Core.idToInt rId