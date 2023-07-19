{-# LANGUAGE OverloadedStrings #-}

module YIBP.Server.Rule (RuleAPI, theRuleAPI) where

import Servant
import Servant.Server.Generic

import Control.Monad.Except

import Data.Vector qualified as V

import GHC.Generics

import Control.Monad.Catch (catch)
import YIBP.Core.Id
import YIBP.Core.Rule
import YIBP.Db
import YIBP.Error
import YIBP.Scheduler
import YIBP.Service.Rule qualified as Service
import YIBP.Validate

data RuleAPI route = RuleAPI
  { _add
      :: route
        :- ReqBody '[JSON] Rule
          :> Post '[JSON] (Id Rule)
  , _update
      :: route
        :- Capture "id" (Id Rule)
          :> ReqBody '[JSON] Rule
          :> Put '[JSON] NoContent
  , _remove
      :: route
        :- Capture "id" (Id Rule)
          :> Delete '[JSON] NoContent
  , _get
      :: route :- Get '[JSON] (V.Vector (IdObject Rule))
  }
  deriving (Generic)

theRuleAPI :: (WithDb, WithScheduler) => RuleAPI (AsServerT Handler)
theRuleAPI =
  RuleAPI
    { _add = addHandler
    , _update = updateHandler
    , _remove = removeHandler
    , _get = getHandler
    }

addHandler :: (WithDb, WithScheduler) => Rule -> Handler (Id Rule)
addHandler r =
  liftIO (Service.createRule r)
    `catch` (\(Service.RuleValidationError details) -> throwValidationError (ValidationErrorWithDetails "rule is invalid" (map show details)))

updateHandler :: (WithDb, WithScheduler) => Id Rule -> Rule -> Handler NoContent
updateHandler rId rule = do
  liftIO (Service.updateRule rId rule)
    `catch` (\Service.RuleDoesNotExist -> raiseServantError (HttpError @Service.RuleDoesNotExist "rule does not exist") err404)
    `catch` (\(Service.RuleValidationError details) -> throwValidationError (ValidationErrorWithDetails "rule is invalid" (map show details)))
  pure NoContent

removeHandler :: (WithDb, WithScheduler) => Id Rule -> Handler NoContent
removeHandler rId = do
  liftIO (Service.deleteRule rId)
    `catch` (\Service.RuleDoesNotExist -> raiseServantError (HttpError @Service.RuleDoesNotExist "rule does not exist") err404)
  pure NoContent

getHandler :: (WithDb) => Handler (V.Vector (IdObject Rule))
getHandler = liftIO Service.getAllRules
