module YIBP.Scheduler
  ( WithScheduler
  , SchedulerRuleEvent (..)
  , Scheduler (..)
  , withScheduler
  , RuleId
  , addRule
  , editRule
  , removeRule
  ) where

import Control.Concurrent.STM
import YIBP.Core.Rule
import YIBP.Core.Id

type RuleId = Int

data SchedulerRuleEvent
  = AddRuleEvent RuleId Rule
  | EditRuleEvent RuleId Rule
  | RemoveRuleEvent RuleId

newtype Scheduler = Scheduler (TQueue SchedulerRuleEvent)

type WithScheduler = (?scheduler :: Scheduler)

withScheduler :: (WithScheduler) => (Scheduler -> a) -> a
withScheduler f = f ?scheduler

addRule :: (WithScheduler) => Id Rule -> Rule -> IO ()
addRule (Id rid) rule = withScheduler $ \(Scheduler q) -> atomically $ do
  writeTQueue q (AddRuleEvent rid rule)

editRule :: (WithScheduler) => Id Rule -> Rule -> IO ()
editRule (Id rid) rule = withScheduler $ \(Scheduler q) -> atomically $ do
  writeTQueue q (EditRuleEvent rid rule)

removeRule :: (WithScheduler) => Id Rule -> IO ()
removeRule (Id rid) = withScheduler $ \(Scheduler q) -> atomically $ do
  writeTQueue q (RemoveRuleEvent rid)
