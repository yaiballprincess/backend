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

import Data.Vector qualified as V

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
