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
import YIBP.Core.Id
import YIBP.Core.Receiver
import YIBP.Core.Rule
import YIBP.Core.Sender
import YIBP.Db
import YIBP.Db.Rule
import YIBP.Scheduler.Scheduler
import YIBP.Util.WithId

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
      :: route :- Get '[JSON] (V.Vector Rule)
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
addHandler = undefined

updateHandler :: (WithDb, WithScheduler) => Id Rule -> Rule -> Handler NoContent
updateHandler = undefined

removeHandler :: (WithDb, WithScheduler) => Id Rule -> Handler NoContent
removeHandler = undefined

getHandler :: (WithDb) => Handler (V.Vector Rule)
getHandler = undefined
