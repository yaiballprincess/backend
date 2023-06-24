{-# LANGUAGE DeriveAnyClass #-}

module YIBP.Server.Rule (RuleAPI, theRuleAPI) where

import Servant
import Servant.Server.Generic

import Control.Monad.Except

import YIBP.App
import YIBP.Scheduler.Util

import Data.Aeson
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V

import GHC.Generics

import Optics
import YIBP.Core.Poll (PollTemplate)
import YIBP.Core.Receiver
import YIBP.Core.Sender
import YIBP.Db.Util
import YIBP.Util.WithId
import YIBP.Scheduler.Scheduler
import YIBP.Core.Rule
import YIBP.Db.Rule

data AddRegularRuleRequest = AddRegularRuleRequest
  { receiverId :: !Int
  , pollTemplateId :: !Int
  , cronRule :: !MyCronSchedule
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EditRegularRuleRequest = EditRegularRuleRequest
  { ruleId :: !Int
  , receiverId :: !(Maybe Int)
  , pollTemplateId :: !(Maybe Int)
  , cronRule :: !(Maybe MyCronSchedule)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data AddExceptionRuleRequest = AddExceptionRuleRequest
  { regularRuleId :: !Int
  , receiverId :: !Int
  , pollTemplateId :: !Int
  , sendAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data EditExceptionRuleRequest = EditExceptionRuleRequest
  { exceptionRuleId :: !Int
  , regularRuleId :: !(Maybe Int)
  , receiverId :: !(Maybe Int)
  , pollTemplateId :: !(Maybe Int)
  , sendAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GetRegularRuleResponseUnit = GetRegularRuleResponseUnit
  { ruleId :: !Int
  , receiver :: !(WithId Receiver)
  , sender :: !(WithId T.Text)
  , pollTemplate :: !(WithId PollTemplate)
  , cronRule :: !MyCronSchedule
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data GetExceptionRuleResponseUnit = GetExceptionRuleResponseUnit
  { exceptionRuleId :: !Int
  , regularRuleId :: !Int
  , receiver :: !(WithId Receiver)
  , sender :: !(WithId T.Text)
  , pollTemplate :: !(WithId PollTemplate)
  , sendAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

type RegularRuleId = Int
type ExceptionRuleId = Int

data RuleAPI route = RuleAPI
  { _addRegular :: route :- "add-regular" :> ReqBody '[JSON] AddRegularRuleRequest :> Post '[JSON] RegularRuleId
  , _addException :: route :- "add-exception" :> ReqBody '[JSON] AddExceptionRuleRequest :> Post '[JSON] ExceptionRuleId
  , _editRegular :: route :- "edit-regular" :> ReqBody '[JSON] EditRegularRuleRequest :> Post '[JSON] NoContent
  , _editException :: route :- "edit-exception" :> ReqBody '[JSON] EditExceptionRuleRequest :> Post '[JSON] NoContent
  , _removeRegular :: route :- "remove-regular" :> ReqBody '[JSON] RegularRuleId :> Post '[JSON] NoContent
  , _removeException :: route :- "remove-exception" :> ReqBody '[JSON] ExceptionRuleId :> Post '[JSON] NoContent
  , _getRules :: route :- "get-rules" :> Get '[JSON] (V.Vector GetRegularRuleResponseUnit)
  , _getExceptions :: route :- "get-exceptions" :> Get '[JSON] (V.Vector GetExceptionRuleResponseUnit)
  }
  deriving (Generic)

theRuleAPI :: (MonadIO m, MonadError ServerError m, WithScheduler) => RuleAPI (AsServerT (AppT m))
theRuleAPI =
  RuleAPI
    { _addRegular = addRegularHandler
    , _addException = addExceptionHandler
    , _editRegular = editRegularHandler
    , _editException = editExceptionHandler
    , _removeRegular = removeRegularHandler
    , _removeException = removeExceptionHandler
    , _getRules = getRulesHandler
    , _getExceptions = getExceptionRulesHandler
    }

addRegularHandler :: (MonadIO m, MonadError ServerError m, WithDb env m, WithScheduler) => AddRegularRuleRequest -> m RegularRuleId
addRegularHandler req = do
  let rule = RegularRule { receiverId = req ^. #receiverId, pollTemplateId = req ^. #pollTemplateId, cronRule = req ^. #cronRule }
  rid <- insertRule rule
  _ <- liftIO $ withSchedulerM $ \sch -> do
    addRegularRule sch rid rule
  pure rid

addExceptionHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => AddExceptionRuleRequest -> m ExceptionRuleId
addExceptionHandler = undefined

editRegularHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => EditRegularRuleRequest -> m NoContent
editRegularHandler = undefined

editExceptionHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => EditExceptionRuleRequest -> m NoContent
editExceptionHandler = undefined

removeRegularHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => RegularRuleId -> m NoContent
removeRegularHandler = undefined

removeExceptionHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => ExceptionRuleId -> m NoContent
removeExceptionHandler = undefined

getRulesHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => m (V.Vector GetRegularRuleResponseUnit)
getRulesHandler = undefined

getExceptionRulesHandler :: (MonadIO m, MonadError ServerError m, WithDb env m) => m (V.Vector GetExceptionRuleResponseUnit)
getExceptionRulesHandler = undefined