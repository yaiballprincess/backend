module YIBP.Db.PollTemplate
  ( insertPollTemplate
  , PollTemplate (..)
  , insertPollTemplateOption
  , PollTemplateUpdate (..)
  , updatePollTemplate
  , updatePollTemplateOption
  , deletePollTemplate
  , deletePollTemplateOption
  , getAll
  , PollTemplateGet (..)
  , getPollTemplatesByIds
  ) where

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH
import YIBP.Core.Poll qualified as Core
import YIBP.Db.Db

import Data.Int
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V

import GHC.Generics
import GHC.Stack

import Optics

data PollTemplate = PollTemplate
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)

data PollTemplateUpdate = PollTemplateUpdate
  { pollTemplateId :: !Int
  , isMultiple :: !(Maybe Bool)
  , isAnonymous :: !(Maybe Bool)
  , endsAt :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Generic)

insertPollTemplate :: (WithDb, HasCallStack) => PollTemplate -> IO (Maybe Int)
insertPollTemplate tmpl = do
  fmap fromIntegral <$> withConn
    ( Session.run
        ( Session.statement
            ( tmpl ^. #isMultiple
            , tmpl ^. #isAnonymous
            , tmpl ^. #endsAt
            )
            stmt
        )
    )
  where
    stmt :: Statement (Bool, Bool, Maybe UTCTime) (Maybe Int32)
    stmt =
      [TH.maybeStatement|
      insert into "poll_template" ("is_multiple", "is_anonymous", "duration")
      values ($1 :: bool, $2 :: bool, $3 :: TIMESTAMPTZ?)
      returning "id" :: int4
    |]

insertPollTemplateOption :: (WithDb, HasCallStack) => Int -> T.Text -> IO (Maybe Int)
insertPollTemplateOption pollTemplateId text = do
  fmap fromIntegral <$> withConn
    ( Session.run
        (Session.statement (fromIntegral pollTemplateId, text) stmt)
    )
  where
    stmt :: Statement (Int32, T.Text) (Maybe Int32)
    stmt =
      [TH.maybeStatement|
      insert into "poll_option" ("poll_template_id", "data") 
      values ($1 :: int4, $2 :: text)
      returning "id" :: int4
      |]

updatePollTemplate :: (WithDb, HasCallStack) => PollTemplateUpdate -> IO Bool
updatePollTemplate upd = do
  withConnEither
    ( Session.run
        (Session.statement (fromIntegral (upd ^. #pollTemplateId), upd ^. #isMultiple, upd ^. #isAnonymous, upd ^. #endsAt) stmt)
    )
    >>= \case
      Left x -> pure False
      Right _ -> pure True
  where
    stmt :: Statement (Int32, Maybe Bool, Maybe Bool, Maybe UTCTime) ()
    stmt =
      [TH.resultlessStatement|
        update "poll_template" 
        set
          "is_multiple" = COALESCE(($2 :: bool?), "is_multiple"),
          "is_anonymous" = COALESCE(($3 :: bool?), "is_anonymous"),
          "duration" = COALESCE(($4 :: TIMESTAMPTZ?), "duration")
        where "id" = $1 :: int4
    |]

updatePollTemplateOption :: (WithDb, HasCallStack) => Int -> T.Text -> IO Bool
updatePollTemplateOption _id text = do
  withConnEither
    ( Session.run
        (Session.statement (fromIntegral _id, text) stmt)
    )
    >>= \case
      Left _ -> pure False
      Right _ -> pure True
  where
    stmt :: Statement (Int32, T.Text) ()
    stmt =
      [TH.resultlessStatement|
      update "poll_option" set "data" = $2 :: text where "id" = $1 :: int4
    |]

deletePollTemplate :: (WithDb, HasCallStack) => Int -> IO Bool
deletePollTemplate _id = do
  withConnEither (Session.run (Session.statement (fromIntegral _id) stmt)) >>= \case
    Left _ -> pure False
    Right _ -> pure True
  where
    stmt :: Statement Int32 ()
    stmt =
      [TH.resultlessStatement|
      delete from "poll_template" where "id" = $1 :: int4
    |]

deletePollTemplateOption :: (WithDb, HasCallStack) => Int -> IO Bool
deletePollTemplateOption _id = do
  withConnEither (Session.run (Session.statement (fromIntegral _id) stmt)) >>= \case
    Left _ -> pure False
    Right _ -> pure True
  where
    stmt :: Statement Int32 ()
    stmt =
      [TH.resultlessStatement|
      delete from "poll_option" where "id" = $1 :: int4
    |]

data PollTemplateGet = PollTemplateGet
  { isMultiple :: !Bool
  , isAnonymous :: !Bool
  , endsAt :: !(Maybe UTCTime)
  , options :: !(V.Vector (Int, T.Text))
  }
  deriving (Show, Eq, Generic)

getAll :: (WithDb, HasCallStack) => IO (V.Vector (Int, PollTemplateGet))
getAll = do
  vec <- withConn (Session.run (Session.statement () stmt))
  pure $
    V.map
      ( \(_id, isMultiple, isAnonymous, duration, optionsIds, optionsData) ->
          ( fromIntegral _id
          , PollTemplateGet
              { isMultiple = isMultiple
              , isAnonymous = isAnonymous
              , endsAt = duration
              , options = V.zip (V.map fromIntegral optionsIds) optionsData
              }
          )
      )
      vec
  where
    stmt :: Statement () (V.Vector (Int32, Bool, Bool, Maybe UTCTime, V.Vector Int32, V.Vector T.Text))
    stmt =
      [TH.vectorStatement|
        select 
          poll_template.id :: int4,
          "is_multiple" :: bool, 
          "is_anonymous" :: bool,
          "duration" :: TIMESTAMPTZ?,
          array_agg(o.id) :: int4[],
          array_agg(o.data) :: text[]
        from "poll_template"
        right join "poll_option" o ON o.poll_template_id = poll_template.id
        group by poll_template.id
      |]

getPollTemplatesByIds :: (WithDb, HasCallStack) => V.Vector Int -> IO (IntMap Core.PollTemplate)
getPollTemplatesByIds ids = do
  vec <- withConn (Session.run (Session.statement (V.map fromIntegral ids) stmt))
  pure $
    IntMap.fromList $
      V.toList $
        V.map
          ( \(i, isMultiple, isAnonymous, duration, options) ->
              ( fromIntegral i
              , Core.PollTemplate
                  { isMultiple = isMultiple
                  , isAnonymous = isAnonymous
                  , endsAt = duration
                  , options = options
                  }
              )
          ) vec
  where
    stmt :: Statement (V.Vector Int32) (V.Vector (Int32, Bool, Bool, Maybe UTCTime, V.Vector T.Text))
    stmt =
      [TH.vectorStatement|
        select
            poll_template.id :: int4,
            "is_multiple" :: bool, 
            "is_anonymous" :: bool,
            "duration" :: TIMESTAMPTZ?,
            array_agg(o.data) :: text[]
          from "poll_template"
          right join "poll_option" o ON o.poll_template_id = poll_template.id
          where poll_template.id = ANY($1 :: int4[])
          group by poll_template.id
      |]