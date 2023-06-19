module YIBP.Server (theAPI) where

import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic

import Control.Monad.IO.Class

import YIBP.App
import YIBP.Server.Auth (AuthAPI, theAuthAPI)
import Control.Monad.Error.Class (MonadError)
import YIBP.Server.Sender

data API route = API
  { _auth :: route :- "api" :> "auth" :> NamedRoutes AuthAPI
  , _sender :: route :- "api" :> "sender" :> NamedRoutes SenderAPI
  }  deriving (Generic)


theAPI :: (MonadIO m, MonadError ServerError m) => API (AsServerT (AppT m))
theAPI =
  API
    { _auth = theAuthAPI
    , _sender = theSenderAPI
    }
