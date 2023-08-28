module YIBP.Service.PollTemplate where

import Data.Coerce (coerce)

import Control.Exception
import Data.Vector qualified as V
import YIBP.Core.Id
import YIBP.Core.PollTemplate
import YIBP.Db

import YIBP.Db.PollTemplate qualified as Db
import YIBP.Db.PollTemplate.Types

data PollTemplateNotFound = PollTemplateNotFound
  deriving (Show, Eq, Exception)

createPollTemplate :: (WithDb) => CreatePollTemplate -> IO (Id PollTemplate)
createPollTemplate crt = do
  let params =
        InsertPollTemplateParams
          { isMultiple = crt.isMultiple
          , isAnonymous = crt.isAnonymous
          , endsAt = crt.endsAt
          , question = crt.question
          , options = coerce crt.options
          }
  Db.insertPollTemplate params

updatePollTemplate :: (WithDb) => Id PollTemplate -> UpdatePollTemplate -> IO ()
updatePollTemplate ptId upt = do
  let params =
        UpdatePollTemplateParams
          { id = ptId
          , isMultiple = upt.isMultiple
          , isAnonymous = upt.isAnonymous
          , endsAt = upt.endsAt
          , question = upt.question
          , options = coerce upt.options
          }
  Db.updatePollTemplate params >>= \case
    True -> pure ()
    False -> throwIO PollTemplateNotFound

removePollTemplate :: (WithDb) => Id PollTemplate -> IO ()
removePollTemplate ptId = do
  Db.deletePollTemplate ptId >>= \case
    True -> pure ()
    False -> throwIO PollTemplateNotFound

getAllPollTemplates :: (WithDb) => IO (V.Vector PollTemplateFull)
getAllPollTemplates = Db.getAllPollTemplates
