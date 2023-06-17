module YIBP.Server (theAPI) where

import Servant.API
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic

import Control.Monad.IO.Class

import YIBP.App
import YIBP.Server.Auth (AuthAPI, theAuthAPI)
import Control.Monad.Error.Class (MonadError)

data API route = API
  { _auth :: route :- "api" :> "auth" :> NamedRoutes AuthAPI
  }  deriving (Generic)


theAPI :: (MonadIO m, MonadError ServerError m) => API (AsServerT (AppT m))
theAPI =
  API
    { _auth = theAuthAPI
    }
