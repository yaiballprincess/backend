module YIBP.Server (theAPI) where

import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Servant.Auth

import Control.Monad.IO.Class

import YIBP.App
import YIBP.Server.Auth (AuthAPI, theAuthAPI, AuthData, withAuth')
import Control.Monad.Error.Class (MonadError)
import YIBP.Server.Sender
import YIBP.Server.Receiver
import YIBP.Server.PollTemplate
import YIBP.Server.Rule
import YIBP.Scheduler.Scheduler
import YIBP.Config
import YIBP.Db.Db

data API route = API
  { _auth :: route :- "api" :> "auth" :> NamedRoutes AuthAPI
  , _sender :: route :- Auth '[JWT] AuthData :> "api" :> "sender" :> NamedRoutes SenderAPI
  , _receiver :: route :- Auth '[JWT] AuthData :> "api" :> "receiver" :> NamedRoutes ReceiverAPI
  , _pollTemplate :: route :- Auth '[JWT] AuthData :> "api" :> "poll" :> NamedRoutes PollTemplateAPI
  , _rule :: route :- Auth '[JWT] AuthData :> "api" :> "rule" :> NamedRoutes RuleAPI
  }  deriving (Generic)


theAPI :: (WithScheduler, WithDb, WithConfig) => API (AsServerT Handler)
theAPI =
  API
    { _auth = theAuthAPI
    , _sender = flip withAuth' theSenderAPI
    , _receiver = flip withAuth' theReceiverAPI
    , _pollTemplate = flip withAuth' thePollTemplateAPI
    , _rule = flip withAuth' theRuleAPI
    }
