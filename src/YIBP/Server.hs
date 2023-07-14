module YIBP.Server (theAPI) where

import Servant.API
import Servant.API.Generic
import Servant.Auth
import Servant.Server
import Servant.Server.Generic

import YIBP.Config
import YIBP.Core.Auth
import YIBP.Db
import YIBP.Scheduler.Scheduler
import YIBP.Server.PollTemplate
import YIBP.Server.Rule
import YIBP.Server.Sender
import YIBP.Server.Session
import YIBP.Server.User

data API route = API
  { _sender :: route :- Auth '[JWT] AuthData :> "api" :> "senders" :> NamedRoutes SenderAPI
  , _pollTemplate :: route :- Auth '[JWT] AuthData :> "api" :> "poll-templates" :> NamedRoutes PollTemplateAPI
  , _rule :: route :- Auth '[JWT] AuthData :> "api" :> "rules" :> NamedRoutes RuleAPI
  , _user :: route :- Auth '[JWT] AuthData :> "api" :> "users" :> NamedRoutes UserAPI
  , _session :: route :- "api" :> "sessions" :> NamedRoutes SessionAPI
  }
  deriving (Generic)

theAPI :: (WithScheduler, WithDb, WithConfig) => API (AsServerT Handler)
theAPI =
  API
    { _sender = flip withAuth theSenderAPI
    , _pollTemplate = flip withAuth thePollTemplateAPI
    , _rule = flip withAuth theRuleAPI
    , _user = flip withAuth theUserAPI
    , _session = theSessionAPI
    }
