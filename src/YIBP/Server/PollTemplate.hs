{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module YIBP.Server.PollTemplate (PollTemplateAPI, thePollTemplateAPI) where

import Data.Aeson
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V

import Servant
import Servant.Server.Generic

import YIBP.App
import YIBP.Db.PollTemplate
import YIBP.Util.WithId

import GHC.Generics (Generic)

import Optics

import Control.Monad.Except
import Deriving.Aeson
import YIBP.Db

data PollTemplateAddRequest = PollTemplateAddRequest
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[OmitNothingFields] PollTemplateAddRequest

data PollTemplateEditRequest = PollTemplateEditRequest
  { pollTemplateId :: !Int
  , isMultiple :: !(Maybe Bool)
  , isAnonymous :: !(Maybe Bool)
  , endsAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[OmitNothingFields] PollTemplateEditRequest

data PollTemplateAddOptionRequest = PollTemplateAddOptionRequest
  { pollTemplateId :: !Int
  , text :: !T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PollTemplateEditOptionRequest = PollTemplateEditOptionRequest
  { pollTemplateOptionId :: !Int
  , text :: !T.Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PollTemplateGetResponse = PollTemplateGetResponse
  { pollTemplateId :: !Int
  , isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , options :: !(V.Vector (WithId T.Text))
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

type PollTemplateId = Int
type PollTemplateOptionId = Int

data PollTemplateAPI route = PollTemplateAPI
  { _add :: route :- "add" :> ReqBody '[JSON] PollTemplateAddRequest :> Post '[JSON] PollTemplateId
  , _addOption :: route :- "add-option" :> ReqBody '[JSON] PollTemplateAddOptionRequest :> Post '[JSON] PollTemplateOptionId
  , _edit :: route :- "edit" :> ReqBody '[JSON] PollTemplateEditRequest :> Post '[JSON] NoContent
  , _editOption :: route :- "edit-option" :> ReqBody '[JSON] PollTemplateEditOptionRequest :> Post '[JSON] NoContent
  , _remove :: route :- "remove" :> ReqBody '[JSON] PollTemplateId :> Post '[JSON] NoContent
  , _removeOption :: route :- "remove-option" :> ReqBody '[JSON] PollTemplateOptionId :> Post '[JSON] NoContent
  , _get :: route :- "get" :> Get '[JSON] (V.Vector PollTemplateGetResponse)
  }
  deriving (Generic)

thePollTemplateAPI :: (WithDb) => PollTemplateAPI (AsServerT Handler)
thePollTemplateAPI =
  PollTemplateAPI
    { _add = addHandler
    , _addOption = addOptionHandler
    , _edit = editHandler
    , _editOption = editOptionHandler
    , _remove = removeHandler
    , _removeOption = removeOptionHandler
    , _get = getHandler
    }

addHandler
  :: WithDb
  => PollTemplateAddRequest
  -> Handler PollTemplateId
addHandler req = do
  liftIO (insertPollTemplate
    PollTemplate
      { isMultiple = req ^. #isMultiple
      , isAnonymous = req ^. #isAnonymous
      , endsAt = req ^. #endsAt
      })
    >>= \case
      Nothing -> throwError err422
      Just x -> pure x

addOptionHandler
  :: WithDb
  => PollTemplateAddOptionRequest
  -> Handler PollTemplateOptionId
addOptionHandler req = do
  liftIO (insertPollTemplateOption (req ^. #pollTemplateId) (req ^. #text)) >>= \case
    Nothing -> throwError err422
    Just x -> pure x

editHandler :: (WithDb) => PollTemplateEditRequest -> Handler NoContent
editHandler req = do
  liftIO
    ( updatePollTemplate
        PollTemplateUpdate
          { pollTemplateId = req ^. #pollTemplateId
          , isMultiple = req ^. #isMultiple
          , isAnonymous = req ^. #isAnonymous
          , endsAt = req ^. #endsAt
          }
    )
    >>= \case
      True -> pure NoContent
      False -> throwError err422

editOptionHandler
  :: (WithDb)
  => PollTemplateEditOptionRequest
  -> Handler NoContent
editOptionHandler req = do
  liftIO
    ( updatePollTemplateOption
        (req ^. #pollTemplateOptionId)
        (req ^. #text)
    )
    >>= \case
      True -> pure NoContent
      False -> throwError err422

removeHandler
  :: (WithDb)
  => PollTemplateId
  -> Handler NoContent
removeHandler _id = do
  liftIO (deletePollTemplate _id) >>= \case
    True -> pure NoContent
    False -> throwError err422

removeOptionHandler
  :: (WithDb)
  => PollTemplateOptionId
  -> Handler NoContent
removeOptionHandler _id = do
  liftIO (deletePollTemplateOption _id) >>= \case
    True -> pure NoContent
    False -> throwError err422

getHandler
  :: (WithDb)
  => Handler (V.Vector PollTemplateGetResponse)
getHandler = do
  V.map
    ( \(i, g) ->
        PollTemplateGetResponse
          { pollTemplateId = i
          , isMultiple = g ^. #isMultiple
          , isAnonymous = g ^. #isAnonymous
          , endsAt = g ^. #endsAt
          , options = V.map (uncurry WithId) (g ^. #options)
          }
    )
    <$> liftIO getAll