module YIBP.Db.Receiver (insertReceiver, deleteReceiver, getAllReceivers) where

import Data.Bifunctor
import Data.Int
import Data.Text qualified as T
import Data.Vector qualified as V

import GHC.Stack (HasCallStack)

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH
import YIBP.Db.Util

import YIBP.Core.Receiver

import GHC.Generics (Generic)
import Optics

insertReceiver :: (WithDb env m, HasCallStack) => Int -> Receiver -> m (Maybe Int)
insertReceiver senderId receiver = do
  r <-
    withConn $
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

deleteReceiver :: (WithDb env m, HasCallStack) => Int -> m Bool
deleteReceiver receiverId = do
  r <- withConn $ Session.run (Session.statement (fromIntegral receiverId) stmt)
  liftError (fmap (== 1) r)
  where
    stmt :: Statement Int32 Int64
    stmt =
      [TH.rowsAffectedStatement|
    delete from "receiver" where "id" = $1 :: int4
    |]

getAllReceivers :: (WithDb env m, HasCallStack) => m (V.Vector (Int, Receiver))
getAllReceivers = do
  vec <- withConn (Session.run (Session.statement () stmt)) >>= liftError
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