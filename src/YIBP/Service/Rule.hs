module YIBP.Service.Rule where

import Control.Monad.Trans.Maybe
import YIBP.Core.Id
import YIBP.Core.PollTemplate
import YIBP.Core.Rule
import YIBP.Core.Sender
import YIBP.Crypto
import YIBP.Db
import YIBP.Db.PollTemplate qualified as Db
import YIBP.Db.Rule qualified as Db
import YIBP.Db.Rule.Types
import YIBP.Db.Sender qualified as Db
import YIBP.Scheduler (WithScheduler)
import YIBP.Scheduler qualified as S
import YIBP.Scheduler.Util qualified as S
import YIBP.Service.Sender qualified as Service

import Control.Exception (Exception, throwIO)
import Data.Foldable
import Data.Time
import Data.Vector qualified as V

data RuleDoesNotExist = RuleDoesNotExist
  deriving (Show, Eq, Exception)

getDetailedRegularRule :: (WithDb) => (Id SenderTag, Id PollTemplate) -> IO (Maybe (Sender 'Decrypted, PollTemplate))
getDetailedRegularRule (sId, pId) = runMaybeT $ do
  sender <- MaybeT $ Service.getSenderById sId
  pt <- MaybeT $ Db.getPollTemplateById pId
  let pt' =
        PollTemplate
          { isMultiple = pt.isMultiple
          , isAnonymous = pt.isAnonymous
          , endsAt = pt.endsAt
          , question = pt.question
          , options = V.map ((.value)) pt.options
          }
  pure (Service.decryptSender sender, pt')

getAllActiveRules :: (WithDb) => IO (V.Vector (Id Rule, Rule))
getAllActiveRules = V.catMaybes . V.map tr <$> Db.getAllRules
  where
    tr r
      | r.isActive && r.canTrigger = Just (r.id, Rule {metadata = r.metadata, isActive = r.isActive})
      | otherwise = Nothing

data RuleValidationErrorDetails
  = InvalidSender
  | InvalidPollTemplate
  | PollTemplateIsEmpty
  | InvalidRegularRule
  | InvalidTime
  deriving (Show)

newtype RuleValidationError = RuleValidationError [RuleValidationErrorDetails]
  deriving stock (Show)
  deriving anyclass (Exception)

validatePollTemplate :: (WithDb) => Id PollTemplate -> IO (Maybe [RuleValidationErrorDetails])
validatePollTemplate ptId =
  Db.getPollTemplateById ptId >>= \case
    Just t
      | V.null t.options -> pure $ Just [PollTemplateIsEmpty]
      | otherwise -> pure Nothing
    Nothing -> pure $ Just [InvalidPollTemplate]

validateCronMatch :: (WithDb) => Id Rule -> UTCTime -> IO (Maybe [RuleValidationErrorDetails])
validateCronMatch regularRuleId t =
  Db.getRuleById regularRuleId >>= \case
    Just (RawRule _ (RMRegular rr) _ _)
      | S.scheduleMatches rr.schedule t -> pure Nothing
      | otherwise -> pure $ Just [InvalidTime]
    _ -> pure $ Just [InvalidRegularRule]

validateRule :: (WithDb) => Rule -> IO (Maybe [RuleValidationErrorDetails])
validateRule (Rule (RMRegular rule) _) = do
  senderError <-
    Db.getSenderById rule.senderId >>= \case
      Just _ -> pure Nothing
      Nothing -> pure $ Just [InvalidSender]
  pollTemplateError <- validatePollTemplate rule.pollTemplateId
  pure $ fold [senderError, pollTemplateError]
validateRule (Rule (RMIgnore rule) _) = do
  validateCronMatch rule.regularRuleId rule.sendAt
validateRule (Rule (RMReplace rule) _) = do
  timeError <- validateCronMatch rule.regularRuleId rule.sendAt
  pollTemplateError <- validatePollTemplate rule.newPollTemplateId
  pure $ fold [timeError, pollTemplateError]

createRule :: (WithScheduler, WithDb) => Rule -> IO (Id Rule)
createRule rule = do
  validateRule rule >>= \case
    Just errors -> throwIO $ RuleValidationError errors
    Nothing -> do
      rId <- Db.insertRule rule
      S.addRule rId rule
      pure rId

updateRule :: (WithDb, WithScheduler) => Id Rule -> Rule -> IO ()
updateRule rId rule = do
  validateRule rule >>= \case
    Just errors -> throwIO $ RuleValidationError errors
    Nothing -> do
      Db.updateRule rId rule >>= \case
        False -> throwIO RuleDoesNotExist
        True -> S.editRule rId rule

deleteRule :: (WithDb, WithScheduler) => Id Rule -> IO ()
deleteRule rId = do
  Db.deleteRule rId >>= \case
    False -> throwIO RuleDoesNotExist
    True -> S.removeRule rId

getAllRules :: (WithDb) => IO (V.Vector (IdObject Rule))
getAllRules = V.map tr <$> Db.getAllRulesCanTrigger
  where
    tr r = IdObject {id = r.id, value = Rule {metadata = r.metadata, isActive = r.isActive}}

markRulesObsolete :: (WithDb) => V.Vector (Id Rule) -> IO ()
markRulesObsolete = Db.setCanTriggerFalseBatch
