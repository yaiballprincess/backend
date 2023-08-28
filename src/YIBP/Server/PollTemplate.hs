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
  , _update
      :: route
        :- Capture "id" (Id PollTemplate)
          :> ReqBody '[JSON] UpdatePollTemplate
          :> Put '[JSON] NoContent
  , _remove
      :: route
        :- Capture "id" (Id PollTemplate)
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
    , _update = updateHandler
    , _remove = removeHandler
    , _get = getHandler
    }

addHandler :: (WithDb) => CreatePollTemplate -> Handler (Id PollTemplate)
addHandler crt = liftIO $ Service.createPollTemplate crt

updateHandler :: (WithDb) => Id PollTemplate -> UpdatePollTemplate -> Handler NoContent
updateHandler ptId upt = do
  liftIO (Service.updatePollTemplate ptId upt)
    `catch` (\(_ :: Service.PollTemplateNotFound) -> raiseServantError (HttpError @Service.PollTemplateNotFound "poll template not found") err404)
  pure NoContent

removeHandler :: (WithDb) => Id PollTemplate -> Handler NoContent
removeHandler ptId = do
  liftIO (Service.removePollTemplate ptId)
    `catch` (\(_ :: Service.PollTemplateNotFound) -> raiseServantError (HttpError @Service.PollTemplateNotFound "poll template not found") err404)
  pure NoContent

getHandler :: (WithDb) => Handler (V.Vector PollTemplateFull)
getHandler = liftIO Service.getAllPollTemplates
