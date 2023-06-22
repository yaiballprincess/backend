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

data API route = API
  { _auth :: route :- "api" :> "auth" :> NamedRoutes AuthAPI
  , _sender :: route :- Auth '[JWT] AuthData :> "api" :> "sender" :> NamedRoutes SenderAPI
  , _receiver :: route :- Auth '[JWT] AuthData :> "api" :> "receiver" :> NamedRoutes ReceiverAPI
  }  deriving (Generic)


theAPI :: (MonadIO m, MonadError ServerError m) => API (AsServerT (AppT m))
theAPI =
  API
    { _auth = theAuthAPI
    , _sender = flip withAuth' theSenderAPI
    , _receiver = flip withAuth' theReceiverAPI
    }
