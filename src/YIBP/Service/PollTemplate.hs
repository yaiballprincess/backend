module YIBP.Service.PollTemplate where

import Control.Exception
import Data.Vector qualified as V
import YIBP.Core.Id
import YIBP.Core.PollTemplate
import YIBP.Db

import YIBP.Db.PollTemplate qualified as Db
import YIBP.Db.PollTemplate.Types

data PollTemplateNotFound = PollTemplateNotFound
  deriving (Show, Eq, Exception)

data PollTemplateOptionNotFound = PollTemplateOptionNotFound
  deriving (Show, Eq, Exception)

createPollTemplate :: (WithDb) => CreatePollTemplate -> IO (Id PollTemplate)
createPollTemplate crt = do
  let params =
        InsertPollTemplateParams
          { isMultiple = crt.isMultiple
          , isAnonymous = crt.isAnonymous
          , endsAt = crt.endsAt
          , question = crt.question
          }
  Db.insertPollTemplate params

addPollTemplateOption :: (WithDb) => Id PollTemplate -> PollTemplateOption -> IO (Id PollTemplateOption)
addPollTemplateOption ptId po =
  Db.insertPollTemplateOption ptId po
    `catch` (\(_ :: DbException) -> throwIO PollTemplateNotFound)

updatePollTemplate :: (WithDb) => Id PollTemplate -> UpdatePollTemplate -> IO ()
updatePollTemplate ptId upt = do
  let params = UpdatePollTemplateParams {id = ptId, isMultiple = upt.isMultiple, isAnonymous = upt.isAnonymous, endsAt = upt.endsAt, question = upt.question}
  Db.updatePollTemplate params >>= \case
    True -> pure ()
    False -> throwIO PollTemplateNotFound

updatePollTemplateOption :: (WithDb) => Id PollTemplate -> Id PollTemplateOption -> PollTemplateOption -> IO ()
updatePollTemplateOption ptId ptoId pto = do
  Db.updatePollTemplateOption ptId ptoId pto >>= \case
    True -> pure ()
    False -> throwIO PollTemplateOptionNotFound

removePollTemplate :: (WithDb) => Id PollTemplate -> IO ()
removePollTemplate ptId = do
  Db.deletePollTemplate ptId >>= \case
    True -> pure ()
    False -> throwIO PollTemplateNotFound

removePollTemplateOption :: (WithDb) => Id PollTemplate -> Id PollTemplateOption -> IO ()
removePollTemplateOption ptId ptoId = do
  Db.deletePollTemplateOption ptId ptoId >>= \case
    True -> pure ()
    False -> throwIO PollTemplateOptionNotFound

getAllPollTemplates :: (WithDb) => IO (V.Vector PollTemplateFull)
getAllPollTemplates = Db.getAllPollTemplates
