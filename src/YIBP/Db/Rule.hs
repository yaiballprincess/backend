module YIBP.Db.Rule where

import Hasql.Session qualified as Session
import Hasql.Statement
import Hasql.TH qualified as TH

import Data.Int (Int32)
import Data.Text qualified as T

import YIBP.Core.Rule
import YIBP.Db.Util

import Optics
import YIBP.Scheduler.Util

-- TODO: add error handling/think of possible errors (such as no references to another column in table)

insertRule :: (WithDb env m) => RegularRule -> m Int
insertRule rule = do
  fromIntegral <$> withConn'
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