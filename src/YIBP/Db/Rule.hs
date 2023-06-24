module YIBP.Db.Rule where

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH

import Data.Int (Int32)
import Data.Text qualified as T
import Data.Vector qualified as V

import YIBP.Core.Rule
import YIBP.Db.Util

import Data.Time
import Optics
import YIBP.Scheduler.Util

-- TODO: add error handling/think of possible errors (such as no references to another column in table)

insertRule :: (WithDb env m) => RegularRule -> m Int
insertRule rule = do
  fromIntegral
    <$> withConn'
      ( Session.run
          ( Session.statement
              ( fromIntegral (rule ^. #receiverId)
              , fromIntegral (rule ^. #pollTemplateId)
              , serializeCronSchedule (rule ^. #cronRule)
              )
              stmt
          )
      )
  where
    stmt :: Statement (Int32, Int32, T.Text) Int32
    stmt =
      [TH.singletonStatement|
      insert into "regular_rule" ("receiver_id", "poll_template_id", "cron_rule")
      values ($1 :: int4, $2 :: int4, $3 :: text)
      returning "id" :: int4
    |]

getAllRules :: (WithDb env m) => m (V.Vector (Int, RegularRule))
getAllRules = do
  vec <- withConn' (Session.run (Session.statement () stmt))
  pure $
    V.map
      ( \(i, receiverId, pollTemplateId, cronRule) ->
          ( fromIntegral i
          , RegularRule
              { receiverId = fromIntegral receiverId
              , pollTemplateId = fromIntegral pollTemplateId
              , cronRule = let Right res = parseMyCronSchedule cronRule in res
              }
          )
      )
      vec
  where
    stmt :: Statement () (V.Vector (Int32, Int32, Int32, T.Text))
    stmt =
      [TH.vectorStatement|
      select "id" :: int4, "receiver_id" :: int4, "poll_template_id" :: int4, "cron_rule" :: text
      from "regular_rule"
    |]

getAllExceptionRules :: (WithDb env m) => m (V.Vector (Int, ExceptionRule))
getAllExceptionRules = do
  vec <- withConn' (Session.run (Session.statement () stmt))
  pure $
    V.map
      ( \(i, regularRuleId, receiverId, pollTemplateId, sendAt) ->
          ( fromIntegral i
          , ExceptionRule
              { regularRuleId = fromIntegral regularRuleId
              , receiverId = fromIntegral receiverId
              , pollTemplateId = fromIntegral pollTemplateId
              , sendAt = sendAt
              }
          )
      )
      vec
  where
    stmt :: Statement () (V.Vector (Int32, Int32, Int32, Int32, UTCTime))
    stmt =
      [TH.vectorStatement|
      select "id" :: int4, "regular_rule_id" :: int4, "receiver_id" :: int4, "poll_template_id" :: int4, "send_at" :: TIMESTAMPTZ
      from "regular_rule"
    |]