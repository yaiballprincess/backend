module YIBP.Service.User
  ( NotEnoughPermissionsError (..)
  , UserConflictError (..)
  , registerUser
  , makePassword
  ) where

import Control.Exception
import GHC.Stack
import YIBP.Core.Auth
import YIBP.Core.Id
import YIBP.Core.User
import YIBP.Db
import YIBP.Db.User
import YIBP.Db.User.Types

import Data.ByteString qualified as BS
import Data.Password.Bcrypt
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

data NotEnoughPermissionsError = NotEnoughPermissionsError
  deriving (Show, Eq, Exception)

checkUserHasPermissions :: (WithDb, HasCallStack) => Id User -> IO ()
checkUserHasPermissions uid =
  getUserById uid >>= \case
    Just rawUser ->
      if rawUser.isAdmin
        then pure ()
        else throwIO NotEnoughPermissionsError
    Nothing -> error "unexpected: user does not exist"
data UserConflictError = UserConflictError
  deriving (Show, Eq, Exception)

makePassword :: T.Text -> IO BS.ByteString
makePassword password = do
  PasswordHash hashedPassword <- hashPassword $ mkPassword password
  pure $ T.encodeUtf8 hashedPassword

registerUser :: (WithDb) => AuthData -> CreateUser -> IO (Id User)
registerUser authData user = do
  _ <- checkUserHasPermissions authData.userId
  hashedPassword <- makePassword user.password
  insertUser
    InsertUser
      { username = user.username
      , hashedPassword = hashedPassword
      , isAdmin = False
      }
    >>= \case
      Just i -> pure i
      Nothing -> throwIO UserConflictError
