module YIBP.Db.Receiver (getReceiversWithSendersByIds, insertReceiver, deleteReceiver, getAllReceivers) where

import Data.Bifunctor
import Data.Int
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Vector qualified as V

import GHC.Stack (HasCallStack)

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH
import YIBP.Db

import YIBP.Core.Receiver

import GHC.Generics (Generic)
import Optics
import YIBP.Core.Sender

insertReceiver :: (WithDb, HasCallStack) => Int -> Receiver -> IO (Maybe Int)
insertReceiver senderId receiver = do
  r <-
    withConnEither $
      Session.run
        ( Session.statement
            ( fromIntegral senderId
            , receiver ^. #name
            , fromIntegral (receiver ^. #peerId)
            )
            stmt
        )
  case r of
    Left _ -> pure Nothing
    Right e -> pure $ Just $ fromIntegral e
  where
    stmt :: Statement (Int32, T.Text, Int32) Int32
    stmt =
      [TH.singletonStatement|
      insert into "receiver" ("sender_id", "name", "peer_id") values
        ($1 :: int4, $2 :: text, $3 :: int)
      returning ("id" :: int4)
    |]

deleteReceiver :: (WithDb, HasCallStack) => Int -> IO Bool
deleteReceiver receiverId = do
  r <- withConn $ Session.run (Session.statement (fromIntegral receiverId) stmt)
  pure $ r == 1
  where
    stmt :: Statement Int32 Int64
    stmt =
      [TH.rowsAffectedStatement|
    delete from "receiver" where "id" = $1 :: int4
    |]

getAllReceivers :: (WithDb, HasCallStack) => IO (V.Vector (Int, Receiver))
getAllReceivers = do
  vec <- withConn (Session.run (Session.statement () stmt))
  pure $
    V.map
      (\(idx, name, peerId) -> (fromIntegral idx, Receiver {name = name, peerId = fromIntegral peerId}))
      vec
  where
    stmt :: Statement () (V.Vector (Int32, T.Text, Int32))
    stmt =
      [TH.vectorStatement|
    select "id" :: int4, "name" :: text, "peer_id" :: int4
    from "receiver"
    |]

getReceiversWithSendersByIds :: (WithDb, HasCallStack) => V.Vector Int -> IO (IntMap (Receiver, Sender))
getReceiversWithSendersByIds ids = do
  vec <- withConn (Session.run (Session.statement (V.map fromIntegral ids) stmt))
  pure $
    IntMap.fromList $
      V.toList $
        V.map
            ( \(i, rName, peerId, sName, accessToken, botAccessToken) ->
                ( fromIntegral i
                ,
                  ( Receiver
                      { name = rName
                      , peerId = fromIntegral peerId
                      }
                  , Sender
                      { name = sName
                      , accessToken = accessToken
                      , botAccessToken = botAccessToken
                      }
                  )
                )
            )
            vec
  where
    stmt :: Statement (V.Vector Int32) (V.Vector (Int32, T.Text, Int32, T.Text, T.Text, Maybe T.Text))
    stmt =
      [TH.vectorStatement|
      select r.id :: int4, r.name :: text, r.peer_id :: int4, s.name :: text, s.access_token :: text, s.bot_access_token :: text?
      from "receiver" as r
      inner join "sender" s ON s.id = r.sender_id
      where r.id = ANY($1 :: int4[])
    |]