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
import YIBP.Core.Rule
import YIBP.Core.Sender
import YIBP.Db.Rule
import YIBP.Db
import YIBP.Scheduler.Scheduler
import YIBP.Util.WithId

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
  { id :: !Int
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

theRuleAPI :: (WithDb, WithScheduler) => RuleAPI (AsServerT Handler)
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

addRegularHandler :: (WithDb, WithScheduler) => AddRegularRuleRequest -> Handler RegularRuleId
addRegularHandler req = do
  let rule = RegularRule {receiverId = req ^. #receiverId, pollTemplateId = req ^. #pollTemplateId, cronRule = req ^. #cronRule}
  rid <- liftIO $ insertRule rule
  _ <- liftIO $ withScheduler $ \sch -> do
    addRegularRule sch rid rule
  pure rid

addExceptionHandler :: (WithDb, WithScheduler) => AddExceptionRuleRequest -> Handler ExceptionRuleId
addExceptionHandler req = do
  let rule = ExceptionRule {regularRuleId = req ^. #regularRuleId, receiverId = req ^. #receiverId, pollTemplateId = req ^. #pollTemplateId, sendAt = req ^. #sendAt}
  regularRule <- liftIO $ getRuleById req.regularRuleId
  tz <- liftIO getCurrentTimeZone
  unless (scheduleMatches regularRule.cronRule (localTimeToUTC utc (utcToLocalTime tz rule.sendAt))) $ do
    throwError err422
  rid <- liftIO $ insertExceptionRule rule
  _ <- liftIO $ withScheduler $ \sch -> do
    addExceptionRule sch rid rule
  pure rid

editRegularHandler :: (WithDb, WithScheduler) => EditRegularRuleRequest -> Handler NoContent
editRegularHandler req = do
  let rule = UpdateRegularRule {ruleId = req.ruleId, receiverId = req.receiverId, pollTemplateId = req.pollTemplateId, cronRule = req.cronRule}
  liftIO (updateRegularRule rule) >>= \case
    Just regularRule -> do
      _ <- liftIO $ withScheduler $ \sch -> do
        editRegularRule sch req.ruleId regularRule
      pure NoContent
    Nothing -> throwError err422

editExceptionHandler :: (WithDb, WithScheduler) => EditExceptionRuleRequest -> Handler NoContent
editExceptionHandler req = do
  let rule = UpdateExceptionRule {exceptionRuleId = req.exceptionRuleId, regularRuleId = req.regularRuleId, receiverId = req.receiverId, pollTemplateId = req.pollTemplateId, sendAt = req.sendAt}
  regularRule <- liftIO $ getRegularRuleByExceptionId req.exceptionRuleId
  _ <- case req.sendAt of
    Just sendAt -> do
      tz <- liftIO getCurrentTimeZone
      unless (scheduleMatches regularRule.cronRule (localTimeToUTC utc (utcToLocalTime tz sendAt))) $ do
        throwError err422
      undefined
    Nothing -> pure ()
  liftIO (updateExceptionRule rule) >>= \case
    Just exceptionRule -> do
      _ <- liftIO $ withScheduler $ \sch -> do
        editExceptionRule sch req.exceptionRuleId exceptionRule
      pure NoContent
    Nothing -> throwError err422

removeRegularHandler :: ( WithDb, WithScheduler) => RegularRuleId -> Handler NoContent
removeRegularHandler _id = do
  _ <- liftIO $ deleteRegularRule _id
  _ <- liftIO $ withScheduler $ \sch -> do
    removeRegularRule sch _id
  pure NoContent

removeExceptionHandler :: (MonadIO m, MonadError ServerError m, WithDb, WithScheduler) => ExceptionRuleId -> m NoContent
removeExceptionHandler _id = do
  _ <- liftIO $ deleteExceptionRule _id
  _ <- liftIO $ withScheduler $ \sch -> do
    removeExceptionRule sch _id
  pure NoContent

getRulesHandler :: (WithDb) => Handler (V.Vector GetRegularRuleResponseUnit)
getRulesHandler = do
  V.map
    ( \(i, rule) ->
        GetRegularRuleResponseUnit
          { ruleId = i
          , receiver = uncurry WithId rule.receiver
          , sender = uncurry WithId rule.sender
          , pollTemplate = uncurry WithId rule.pollTemplate
          , cronRule = rule.cronRule
          }
    )
    <$> liftIO getAllRulesDetailed

getExceptionRulesHandler :: (WithDb) => Handler (V.Vector GetExceptionRuleResponseUnit)
getExceptionRulesHandler = do
  V.map
    ( \(i, rule) ->
        GetExceptionRuleResponseUnit
          { id = i
          , regularRuleId = rule.regularRuleId
          , receiver = uncurry WithId rule.receiver
          , sender = uncurry WithId rule.sender
          , pollTemplate = uncurry WithId rule.pollTemplate
          , sendAt = rule.sendAt
          }
    )
    <$> liftIO getAllExceptionRulesDetailed