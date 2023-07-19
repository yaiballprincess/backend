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
import YIBP.Service.Sender qualified as Service
import YIBP.Scheduler (WithScheduler)
import YIBP.Scheduler qualified as S

import Data.Vector qualified as V
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class

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

createRule :: (WithScheduler, WithDb) => Rule -> IO (Id Rule)
createRule rule = do
  rId <- Db.insertRule rule
  S.addRule rId rule
  pure rId

updateRule :: (WithDb, WithScheduler) => Id Rule -> Rule -> IO ()
updateRule rId rule = do
  Db.updateRule rId rule >>= \case
    False -> throwIO RuleDoesNotExist
    True -> S.editRule rId rule

deleteRule :: (WithDb, WithScheduler) => Id Rule -> IO ()
deleteRule rId = do
  Db.deleteRule rId >>= \case
    False -> throwIO RuleDoesNotExist
    True -> S.removeRule rId

getAllRules :: WithDb => IO (V.Vector (IdObject Rule))
getAllRules = V.map tr <$> Db.getAllRulesCanTrigger
  where
    tr r = IdObject { id = r.id, value = Rule { metadata = r.metadata, isActive = r.isActive }}

markRulesObsolete :: WithDb => V.Vector (Id Rule) -> IO ()
markRulesObsolete = Db.setCanTriggerFalseBatch
