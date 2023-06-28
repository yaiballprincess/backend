module YIBP.Db.Sender
  ( insertSender
  , getAllSenders
  , getSenderById
  , deleteSender
  , UpdateSenderPayload (..)
  , updateSender
  ) where

import Data.Bifunctor
import Data.Int
import Data.Text qualified as T
import Data.Vector qualified as V

import GHC.Stack (HasCallStack)

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH
import YIBP.Db

import YIBP.Core.Sender

import GHC.Generics (Generic)
import Optics

insertSenderSession :: Sender -> Session.Session (Maybe Int)
insertSenderSession sender =
  fmap fromIntegral
    <$> Session.statement
      ( sender ^. #name
      , sender ^. #accessToken
      , sender ^. #botAccessToken
      )
      stmt
  where
    
    stmt =
      [TH.maybeStatement|
      insert into "sender" ("name", "access_token", "bot_access_token") values
        ($1 :: text, $2 :: text, $3 :: text?)
      on conflict do nothing
      returning ("id" :: int4)
    |]

insertSender :: (WithDb, HasCallStack) => Sender -> IO (Maybe Int)
insertSender sender = do
  withConn $ Session.run (insertSenderSession sender)

getAllSendersSession :: Session.Session (V.Vector (Int, T.Text))
getAllSendersSession =
  V.map (first fromIntegral)
    <$> Session.statement () stmt
  where
    stmt :: Statement () (V.Vector (Int32, T.Text))
    stmt =
      [TH.vectorStatement|
      select ("id" :: int4), ("name" :: text)
      from "sender"
    |]

getAllSenders :: (WithDb, HasCallStack) => IO (V.Vector (Int, T.Text))
getAllSenders = withConn (Session.run getAllSendersSession)

getSenderById :: (WithDb, HasCallStack) => Int -> IO (Maybe Sender)
getSenderById sid = do
  r <- withConn (Session.run (Session.statement (fromIntegral sid) stmt))
  pure $ (\(n, at_, bat) -> Sender {name = n, accessToken = at_, botAccessToken = bat}) <$> r
  where
    stmt :: Statement Int32 (Maybe (T.Text, T.Text, Maybe T.Text))
    stmt =
      [TH.maybeStatement|
      select "name" :: text, "access_token" :: text, "bot_access_token" :: text?
      from "sender"
      where "id" = $1 :: int4
    |]

deleteSenderSession :: Int -> Session.Session Bool
deleteSenderSession _id = (== 1) <$> Session.statement (fromIntegral _id) stmt
  where
    stmt :: Statement Int32 Int64
    stmt =
      [TH.rowsAffectedStatement|
      DELETE FROM "sender" WHERE "id" = ($1 :: int4)
    |]

deleteSender :: (WithDb, HasCallStack) => Int -> IO Bool
deleteSender _id = withConn (Session.run (deleteSenderSession _id))

data UpdateSenderPayload = UpdateSenderPayload
  { newName :: !(Maybe T.Text)
  , newAccessToken :: !(Maybe T.Text)
  , newBotAccessToken :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic)

makeFieldsNoPrefix ''UpdateSenderPayload

updateSenderSession :: Int -> UpdateSenderPayload -> Session.Session Bool
updateSenderSession _id payload =
  (== 1)
    <$> Session.statement
      ( fromIntegral _id
      , payload ^. #newName
      , payload ^. #newAccessToken
      , payload ^. #newBotAccessToken
      )
      stmt
  where
    stmt :: Statement (Int32, Maybe T.Text, Maybe T.Text, Maybe T.Text) Int64
    stmt =
      [TH.rowsAffectedStatement|
      UPDATE "sender"
      SET
        "name" = COALESCE(($2 :: text?), "name"),
        "access_token" = COALESCE(($3 :: text?), "access_token"),
        "bot_access_token" = COALESCE(($4 :: text?), "bot_access_token")
      WHERE 
        "id" = $1 :: int4 AND
        NOT EXISTS (SELECT 1 FROM "sender" WHERE "name" = ($2 :: text?))
    |]

updateSender :: (WithDb, HasCallStack) => Int -> UpdateSenderPayload -> IO Bool
updateSender _id payload = withConn (Session.run (updateSenderSession _id payload))