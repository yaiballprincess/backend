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
import YIBP.Core.Poll
import YIBP.Core.Receiver
import YIBP.Scheduler.Util

-- TODO: add error handling/think of possible errors (such as no references to another column in table)

getRuleById :: (WithDb env m) => Int -> m RegularRule
getRuleById _id = do
  withConn' (Session.run (Session.statement (fromIntegral _id) stmt)) >>= \case
    (receiverId, pollTemplateId, cronRule) ->
      pure $
        RegularRule
          { receiverId = fromIntegral receiverId
          , pollTemplateId = fromIntegral pollTemplateId
          , cronRule = let Right res = parseMyCronSchedule cronRule in res
          }
  where
    stmt :: Statement Int32 (Int32, Int32, T.Text)
    stmt =
      [TH.singletonStatement|
      select "receiver_id" :: int4, "poll_template_id" :: int4, "cron_rule" :: text
      from "regular_rule"
      where "id" = $1 ::  int4
    |]

getRegularRuleByExceptionId :: WithDb env m => Int -> m RegularRule
getRegularRuleByExceptionId _id = do
  withConn' (Session.run (Session.statement (fromIntegral _id) stmt)) >>= \case
    (receiverId, pollTemplateId, cronRule) ->
      pure $
        RegularRule
          { receiverId = fromIntegral receiverId
          , pollTemplateId = fromIntegral pollTemplateId
          , cronRule = let Right res = parseMyCronSchedule cronRule in res
          }
  where
    stmt :: Statement Int32 (Int32, Int32, T.Text)
    stmt =
      [TH.singletonStatement|
      select rr.receiver_id :: int4, rr.poll_template_id :: int4, rr.cron_rule :: text
      from "exception_rule" as er
      inner join "regular_rule" rr ON rr.id = er.regular_rule_id
      where "id" = $1 ::  int4
    |]

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

insertExceptionRule :: (WithDb env m) => ExceptionRule -> m Int
insertExceptionRule rule = do
  fromIntegral
    <$> withConn'
      ( Session.run
          ( Session.statement
              ( fromIntegral (rule ^. #regularRuleId)
              , fromIntegral (rule ^. #receiverId)
              , fromIntegral (rule ^. #pollTemplateId)
              , rule ^. #sendAt
              )
              stmt
          )
      )
  where
    stmt :: Statement (Int32, Int32, Int32, UTCTime) Int32
    stmt =
      [TH.singletonStatement|
      insert into "exception_rule" ("regular_rule_id", "receiver_id", "poll_template_id", "send_at") 
      values ($1 :: int4, $2 :: int4, $3 :: int4, $4 :: TIMESTAMPTZ)
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
      select 
        "id" :: int4, 
        "regular_rule_id" :: int4, 
        "receiver_id" :: int4, 
        "poll_template_id" :: int4, 
        "send_at" :: TIMESTAMPTZ
      from "exception_rule"
    |]

data UpdateRegularRule = UpdateRegularRule
  { ruleId :: !Int
  , receiverId :: !(Maybe Int)
  , pollTemplateId :: !(Maybe Int)
  , cronRule :: !(Maybe MyCronSchedule)
  }

data UpdateExceptionRule = UpdateExceptionRule
  { exceptionRuleId :: !Int
  , regularRuleId :: !(Maybe Int)
  , receiverId :: !(Maybe Int)
  , pollTemplateId :: !(Maybe Int)
  , sendAt :: !(Maybe UTCTime)
  }

updateRegularRule :: (WithDb env m) => UpdateRegularRule -> m (Maybe RegularRule)
updateRegularRule rule = do
  withConn'
    ( Session.run
        ( Session.statement
            ( fromIntegral rule.ruleId
            , fromIntegral <$> rule.receiverId
            , fromIntegral <$> rule.pollTemplateId
            , serializeCronSchedule <$> rule.cronRule
            )
            stmt
        )
    )
    >>= \case
      Just (rId, ptId, cronRule) ->
        pure $
          Just $
            RegularRule
              { receiverId = fromIntegral rId
              , pollTemplateId = fromIntegral ptId
              , cronRule = let Right res = parseMyCronSchedule cronRule in res
              }
      Nothing -> pure Nothing
  where
    stmt =
      [TH.maybeStatement|
      update "regular_rule"
      set
        "receiver_id" = COALESCE($2 :: int4?, "receiver_id"),
        "poll_template_id" = COALESCE($3 :: int4?, "poll_template_id"),
        "cron_rule" = COALESCE($4 :: text?, "cron_rule")
      where "id" = $1 :: int4
      returning
        "receiver_id" :: int4,
        "poll_template_id" :: int4,
        "cron_rule" :: text
    |]

updateExceptionRule :: (WithDb env m) => UpdateExceptionRule -> m (Maybe ExceptionRule)
updateExceptionRule rule = do
  withConn'
    ( Session.run
        ( Session.statement
            ( fromIntegral rule.exceptionRuleId
            , fromIntegral <$> rule.regularRuleId
            , fromIntegral <$> rule.receiverId
            , fromIntegral <$> rule.pollTemplateId
            , rule.sendAt
            )
            stmt
        )
    )
    >>= \case
      Just (rrId, rId, ptId, sendAt) ->
        pure $
          Just $
            ExceptionRule
              { regularRuleId = fromIntegral rrId
              , receiverId = fromIntegral rId
              , pollTemplateId = fromIntegral ptId
              , sendAt = sendAt
              }
      Nothing -> pure Nothing
  where
    stmt =
      [TH.maybeStatement|
      update "exception_rule"
      set
        "regular_rule_id" = COALESCE($2 :: int4?, "regular_rule_id"),
        "receiver_id" = COALESCE($3 :: int4?, "receiver_id"),
        "poll_template_id" = COALESCE($4 :: int4?, "poll_template_id"),
        "send_at" = COALESCE($5 :: TIMESTAMPTZ?, "send_at")
      where "id" = $1 :: int4
      returning
        "regular_rule_id" :: int4,
        "receiver_id" :: int4,
        "poll_template_id" :: int4,
        "send_at" :: TIMESTAMPTZ
    |]

deleteRegularRule :: (WithDb env m) => Int -> m Bool
deleteRegularRule _id = do
  withConn'
    ( Session.run
        ( Session.statement
            (fromIntegral _id)
            stmt
        )
    )
    >>= \case
      1 -> pure True
      _ -> pure False
  where
    stmt =
      [TH.rowsAffectedStatement|
      delete from "regular_rule" where "id" = $1 :: int4
    |]

deleteExceptionRule :: (WithDb env m) => Int -> m Bool
deleteExceptionRule _id = do
  withConn'
    ( Session.run
        ( Session.statement
            (fromIntegral _id)
            stmt
        )
    )
    >>= \case
      1 -> pure True
      _ -> pure False
  where
    stmt =
      [TH.rowsAffectedStatement|
      delete from "exception_rule" where "id" = $1 :: int4
    |]

data RegularRuleDetailed = RegularRuleDetailed
  { receiver :: !(Int, Receiver)
  , sender :: !(Int, T.Text)
  , pollTemplate :: !(Int, PollTemplate)
  , cronRule :: !MyCronSchedule
  }

getAllRulesDetailed :: (WithDb env m) => m (V.Vector (Int, RegularRuleDetailed))
getAllRulesDetailed = do
  vec <- withConn' (Session.run (Session.statement () stmt))
  pure $
    V.map
      ( \(i, receiverId, receiverName, receiverPeerId, senderId, senderName, ptId, ptIsMultiple, ptIsAnonymous, ptDuration, ptOptions, cronRule) ->
          ( fromIntegral i
          , RegularRuleDetailed
              { receiver = (fromIntegral receiverId, Receiver {name = receiverName, peerId = fromIntegral receiverPeerId})
              , sender = (fromIntegral senderId, senderName)
              , pollTemplate = (fromIntegral ptId, PollTemplate {isMultiple = ptIsMultiple, isAnonymous = ptIsAnonymous, endsAt = ptDuration, options = ptOptions})
              , cronRule = let Right res = parseMyCronSchedule cronRule in res
              }
          )
      )
      vec
  where
    stmt :: Statement () (V.Vector (Int32, Int32, T.Text, Int32, Int32, T.Text, Int32, Bool, Bool, Maybe UTCTime, V.Vector T.Text, T.Text))
    stmt =
      [TH.vectorStatement|
      select
        rr.id :: int4,
        r.id :: int4, r.name :: text, r.peer_id :: int4,
        s.id :: int4, s.name :: text,
        pt.id :: int4, pt.is_multiple :: bool, pt.is_anonymous :: bool, pt.duration :: TIMESTAMPTZ?, array_agg(po.data) :: text[],
        rr.cron_rule :: text
      from "regular_rule" as rr
      inner join "receiver" r ON r.id = rr.receiver_id
      inner join "sender" s ON s.id = r.sender_id
      inner join "poll_template" pt ON pt.id = rr.poll_template_id
      inner join "poll_option" po ON po.poll_template_id = pt.id
      group by r.id, rr.id, s.id, pt.id
    |]

data ExceptionRuleDetailed = ExceptionRuleDetailed
  { regularRuleId :: !Int
  , receiver :: !(Int, Receiver)
  , sender :: !(Int, T.Text)
  , pollTemplate :: !(Int, PollTemplate)
  , sendAt :: UTCTime
  }

getAllExceptionRulesDetailed :: (WithDb env m) => m (V.Vector (Int, ExceptionRuleDetailed))
getAllExceptionRulesDetailed = do
  vec <- withConn' (Session.run (Session.statement () stmt))
  pure $
    V.map
      ( \(i, regularRuleId, receiverId, receiverName, receiverPeerId, senderId, senderName, ptId, ptIsMultiple, ptIsAnonymous, ptDuration, ptOptions, sendAt) ->
          ( fromIntegral i
          , ExceptionRuleDetailed
              { regularRuleId = fromIntegral regularRuleId
              , receiver = (fromIntegral receiverId, Receiver {name = receiverName, peerId = fromIntegral receiverPeerId})
              , sender = (fromIntegral senderId, senderName)
              , pollTemplate = (fromIntegral ptId, PollTemplate {isMultiple = ptIsMultiple, isAnonymous = ptIsAnonymous, endsAt = ptDuration, options = ptOptions})
              , sendAt = sendAt
              }
          )
      )
      vec
  where
    stmt :: Statement () (V.Vector (Int32, Int32, Int32, T.Text, Int32, Int32, T.Text, Int32, Bool, Bool, Maybe UTCTime, V.Vector T.Text, UTCTime))
    stmt =
      [TH.vectorStatement|
      select
        er.id :: int4,
        er.regular_rule_id :: int4,
        r.id :: int4, r.name :: text, r.peer_id :: int4,
        s.id :: int4, s.name :: text,
        pt.id :: int4, pt.is_multiple :: bool, pt.is_anonymous :: bool, pt.duration :: TIMESTAMPTZ?, array_agg(po.data) :: text[],
        er.send_at :: TIMESTAMPTZ
      from "exception_rule" as er
      inner join "receiver" r ON r.id = er.receiver_id
      inner join "sender" s ON s.id = r.sender_id
      inner join "poll_template" pt ON pt.id = er.poll_template_id
      inner join "poll_option" po ON po.poll_template_id = pt.id
      group by er.id, r.id, s.id, pt.id
    |]