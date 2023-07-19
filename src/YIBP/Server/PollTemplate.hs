{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.PollTemplate (PollTemplateAPI, thePollTemplateAPI) where

import Data.Vector qualified as V

import Servant
import Servant.Server.Generic

import YIBP.Core.PollTemplate

import Control.Monad.Catch (catch)
import Control.Monad.Except
import Deriving.Aeson
import YIBP.Core.Id
import YIBP.Db
import YIBP.Error
import YIBP.Service.PollTemplate qualified as Service

data PollTemplateAPI route = PollTemplateAPI
  { _add
      :: route
        :- ReqBody '[JSON] CreatePollTemplate
          :> Post '[JSON] (Id PollTemplate)
  , _addOption
      :: route
        :- Capture "id" (Id PollTemplate)
          :> "options"
          :> ReqBody '[JSON] PollTemplateOption
          :> Post '[JSON] (Id PollTemplateOption)
  , _update
      :: route
        :- Capture "id" (Id PollTemplate)
          :> ReqBody '[JSON] UpdatePollTemplate
          :> Put '[JSON] NoContent
  , _updateOption
      :: route
        :- Capture "id" (Id PollTemplate)
          :> "options"
          :> Capture "option-id" (Id PollTemplateOption)
          :> ReqBody '[JSON] PollTemplateOption
          :> Put '[JSON] NoContent
  , _remove
      :: route
        :- Capture "id" (Id PollTemplate)
          :> Delete '[JSON] NoContent
  , _removeOption
      :: route
        :- Capture "id" (Id PollTemplate)
          :> "options"
          :> Capture "option-id" (Id PollTemplateOption)
          :> Delete '[JSON] NoContent
  , _get
      :: route
        :- Get '[JSON] (V.Vector PollTemplateFull)
  }
  deriving (Generic)

thePollTemplateAPI :: (WithDb) => PollTemplateAPI (AsServerT Handler)
thePollTemplateAPI =
  PollTemplateAPI
    { _add = addHandler
    , _addOption = addOptionHandler
    , _update = updateHandler
    , _updateOption = updateOptionHandler
    , _remove = removeHandler
    , _removeOption = removeOptionHandler
    , _get = getHandler
    }

addHandler :: (WithDb) => CreatePollTemplate -> Handler (Id PollTemplate)
addHandler crt = liftIO $ Service.createPollTemplate crt

addOptionHandler :: (WithDb) => Id PollTemplate -> PollTemplateOption -> Handler (Id PollTemplateOption)
addOptionHandler ptId pto =
  liftIO (Service.addPollTemplateOption ptId pto)
    `catch` (\(_ :: Service.PollTemplateNotFound) -> raiseServantError (HttpError @Service.PollTemplateNotFound "poll template not found") err404)

updateHandler :: (WithDb) => Id PollTemplate -> UpdatePollTemplate -> Handler NoContent
updateHandler ptId upt = do
  liftIO (Service.updatePollTemplate ptId upt)
    `catch` (\(_ :: Service.PollTemplateNotFound) -> raiseServantError (HttpError @Service.PollTemplateNotFound "poll template not found") err404)
  pure NoContent

updateOptionHandler :: (WithDb) => Id PollTemplate -> Id PollTemplateOption -> PollTemplateOption -> Handler NoContent
updateOptionHandler ptId ptoId pto = do
  liftIO (Service.updatePollTemplateOption ptId ptoId pto)
    `catch` (\(_ :: Service.PollTemplateOptionNotFound) -> raiseServantError (HttpError @Service.PollTemplateOptionNotFound "poll template option not found") err404)
  pure NoContent

removeHandler :: (WithDb) => Id PollTemplate -> Handler NoContent
removeHandler ptId = do
  liftIO (Service.removePollTemplate ptId)
    `catch` (\(_ :: Service.PollTemplateNotFound) -> raiseServantError (HttpError @Service.PollTemplateNotFound "poll template not found") err404)
  pure NoContent

removeOptionHandler :: (WithDb) => Id PollTemplate -> Id PollTemplateOption -> Handler NoContent
removeOptionHandler ptId ptoId = do
  liftIO (Service.removePollTemplateOption ptId ptoId)
    `catch` (\(_ :: Service.PollTemplateOptionNotFound) -> raiseServantError (HttpError @Service.PollTemplateOptionNotFound "poll template option not found") err404)
  pure NoContent

getHandler :: (WithDb) => Handler (V.Vector PollTemplateFull)
getHandler = liftIO Service.getAllPollTemplates
