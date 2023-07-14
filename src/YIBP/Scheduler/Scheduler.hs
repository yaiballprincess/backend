{-# LANGUAGE OverloadedStrings #-}

module YIBP.Scheduler.Scheduler
  ( Scheduler
  , WithScheduler
  , withScheduler
  , mkScheduler
  , runScheduler
  , addRule
  , editRule
  , removeRule
  , initScheduler
  ) where

import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Vector qualified as V

import System.Random

import Control.Concurrent.STM

import YIBP.Scheduler.Util

import Control.Applicative
import Control.Monad
import GHC.Float.RealFracMethods

import Control.Concurrent

import Optics

import YIBP.Core.Id
import YIBP.Core.PollTemplate
import YIBP.Core.Receiver
import YIBP.Core.Rule
import YIBP.Core.Sender
import YIBP.Crypto
import YIBP.Db
import YIBP.Db.PollTemplate
import YIBP.Db.Receiver
import YIBP.Db.Rule
import YIBP.VK.Client
import YIBP.VK.Types

type RuleId = Int

data SchedulerRuleEvent
  = AddRuleEvent RuleId Rule
  | EditRuleEvent RuleId Rule
  | RemoveRuleEvent RuleId

newtype Scheduler = Scheduler (TQueue SchedulerRuleEvent)

type WithScheduler = (?scheduler :: Scheduler)

withScheduler :: (WithScheduler) => (Scheduler -> a) -> a
withScheduler f = f ?scheduler

mkScheduler :: IO Scheduler
mkScheduler = Scheduler <$> newTQueueIO

addRule :: (WithScheduler) => Id Rule -> Rule -> IO ()
addRule (Id rid) rule = withScheduler $ \(Scheduler q) -> atomically $ do
  writeTQueue q (AddRuleEvent rid rule)

editRule :: (WithScheduler) => Id Rule -> Rule -> IO ()
editRule (Id rid) rule = withScheduler $ \(Scheduler q) -> atomically $ do
  writeTQueue q (EditRuleEvent rid rule)

removeRule :: (WithScheduler) => Id Rule -> IO ()
removeRule (Id rid) = withScheduler $ \(Scheduler q) -> atomically $ do
  writeTQueue q (RemoveRuleEvent rid)

doSTMWithTimeout :: Int -> STM a -> IO (Maybe a)
doSTMWithTimeout n action = do
  delay <- registerDelay n
  atomically $
    Just <$> action
      <|> Nothing <$ (readTVar delay >>= check)

data SchedulerState = SchedulerState
  { nextTime :: !(IntMap UTCTime)
  , regularRules :: !(IntMap RegularRule)
  , ignoreRules :: !(IntMap IgnoreRule)
  , replaceRules :: !(IntMap ReplaceRule)
  }

runScheduler :: (WithDb, WithScheduler) => IO ()
runScheduler = runScheduler' (SchedulerState IntMap.empty IntMap.empty IntMap.empty IntMap.empty)

runScheduler'
  :: (WithDb, WithScheduler)
  => SchedulerState
  -> IO ()
runScheduler' = loop
  where
    Scheduler queue = withScheduler id
    loop state = do
      curTime <- getCurrentTime
      let delay = getDelayToNearestEvent curTime state
      doSTMWithTimeout delay (readTQueue queue) >>= \case
        Nothing -> onExecRule state >>= loop
        Just ev -> onRuleEvent ev state >>= loop

data Candidate = Candidate
  { senderId :: !(Id SenderTag)
  , peerId :: !Int
  , pollTemplateId :: !(Id PollTemplate)
  }

data CandidateFull = CandidateFull
  { sender :: !(Sender 'Decrypted)
  , peerId :: !Int
  , pollTemplate :: !PollTemplate
  }

getExecCandidates :: UTCTime -> SchedulerState -> V.Vector Candidate
getExecCandidates curTime state =
  IntMap.assocs pureRules
    & map processRule
    & V.fromList
  where
    satisfyingRules = IntMap.keysSet $ IntMap.filterWithKey (\_ t -> t <= curTime) state.nextTime
    ignoreRules =
      IntMap.filterWithKey
        ( \_ k -> case IntMap.lookup (idToInt k.regularRuleId) state.nextTime of
            Just t -> k.sendAt == t
            Nothing -> False
        )
        state.ignoreRules
        & IntMap.elems
        & map (\r -> idToInt r.regularRuleId)
        & IntSet.fromList
    pureRuleIds = satisfyingRules `IntSet.difference` ignoreRules
    pureRules = state.regularRules `IntMap.restrictKeys` pureRuleIds

    processRule :: (Int, RegularRule) -> Candidate
    processRule (rId, r) = case IntMap.lookup rId state.replaceRules of
      Just replaceRule
        | replaceRule.sendAt == (state.nextTime IntMap.! rId) ->
            Candidate
              { senderId = r.senderId
              , peerId = r.peerId
              , pollTemplateId = replaceRule.newPollTemplateId
              }
      _ ->
        Candidate
          { senderId = r.senderId
          , peerId = r.peerId
          , pollTemplateId = r.pollTemplateId
          }

sendMessage :: CandidateFull -> IO ()
sendMessage = undefined

getObsoleteRules :: UTCTime -> SchedulerState -> IntSet
getObsoleteRules = undefined

onExecRule :: (WithScheduler) => SchedulerState -> IO SchedulerState
onExecRule state = do
  curTime <- getCurrentTime
  let candidates = getExecCandidates curTime state
  fullCandidates <- V.forM candidates undefined
  V.forM_ fullCandidates sendMessage

  let obsoleteRules = getObsoleteRules curTime state
  forM_ (map Id (IntSet.toList obsoleteRules)) removeRule
  -- set can_trigger = false
  --
  -- update nextTime (state')
  undefined

onRuleEvent :: SchedulerRuleEvent -> SchedulerState -> IO SchedulerState
onRuleEvent (AddRuleEvent rid (Rule (RMRegular rule) _)) state = do
  curTime <- getCurrentTime
  tz <- getCurrentTimeZone
  case calculateNextExecTime rule.schedule curTime tz of
    Just t ->
      pure $
        state
          { nextTime = IntMap.insert rid t state.nextTime
          , regularRules = IntMap.insert rid rule state.regularRules
          }
    Nothing -> pure state -- TODO: mark event: `can_trigger` = false
onRuleEvent (AddRuleEvent rid (Rule (RMIgnore rule) _)) state = do
  pure $
    state
      { ignoreRules = IntMap.insert rid rule state.ignoreRules
      }
onRuleEvent (AddRuleEvent rid (Rule (RMReplace rule) _)) state = do
  pure $
    state
      { replaceRules = IntMap.insert rid rule state.replaceRules
      }
onRuleEvent (EditRuleEvent rid rule) state =
  onRuleEvent (RemoveRuleEvent rid) state >>= onRuleEvent (AddRuleEvent rid rule)
onRuleEvent (RemoveRuleEvent rid) state
  | IntMap.member rid state.regularRules =
      pure $
        state
          { nextTime = IntMap.delete rid state.nextTime
          , regularRules = IntMap.delete rid state.regularRules
          }
  | IntMap.member rid state.ignoreRules =
      pure $
        state
          { ignoreRules = IntMap.delete rid state.ignoreRules
          }
  | IntMap.member rid state.replaceRules =
      pure $
        state
          { replaceRules = IntMap.delete rid state.replaceRules
          }
  | otherwise = pure state

calculateNextExecTime :: MyCronSchedule -> UTCTime -> TimeZone -> Maybe UTCTime
calculateNextExecTime cron t tz = do
  -- [Note]
  --  `localTimeToUTC utc (utcToLocalTime tz t)` returns `UTCTime`
  --  that holds local time
  --  For example, tz = MSK, t = 2023-05-01 09:00:00 UTC (12:00 in MSK)
  --  Then, the result is 2023-05-01 12:00:00 UTC.
  --  This hack is needed to correctly calculate next time when to wake up.
  --  Additionally, there's similar transformation.
  --
  --  Assume, cron rule is 'Every day at 16:00'.
  --  For now, the desired behavior 'at 16:00 MSK'.
  --  I think it's controversial design decision. For example, cron rules in
  --  GitHub Actions should be written with UTC in mind. However, for end-user
  --  it might be hard to form such cron rule, but it could be solved on frontend.
  --
  --  **I assume** this should work properly if offset does not change, however,
  --  if this is the case, then this function should be re-written.
  --  Or cron rules should be specified in UTC. TODO, anyway.
  time <- nextMatch cron (localTimeToUTC utc (utcToLocalTime tz t))
  pure $ localTimeToUTC tz (utcToLocalTime utc time)

getDelayToNearestEvent :: UTCTime -> SchedulerState -> Int
getDelayToNearestEvent curTime m = case Set.lookupMin (Set.fromList (IntMap.elems m.nextTime)) of
  Just t ->
    let diffTime = diffUTCTime t curTime
    in  if diffTime < 0
          then 0
          else floorDoubleInt $ realToFrac diffTime * 1000000
  Nothing -> maxBound

--
--     getCandidates
--       :: UTCTime
--       -> Map RuleId UTCTime
--       -> Map RuleId RegularRule
--       -> Map ExceptionRuleId ExceptionRule
--       -> [(RuleId, ReceiverId, PollTemplateId)]
--     getCandidates curTime m rulesMap exceptionsMap = map processElement candidates
--       where
--         candidates = Map.assocs $ Map.filter (< curTime) m
--
--         tryFindException :: RuleId -> UTCTime -> Maybe ExceptionRule
--         tryFindException rid t =
--           find (\er -> (er.regularRuleId == rid) && (er.sendAt == t)) (Map.elems exceptionsMap)
--
--         processElement :: (RuleId, UTCTime) -> (RuleId, ReceiverId, PollTemplateId)
--         processElement (rid, t) = case tryFindException rid t of
--           Just er -> (rid, er.receiverId, er.pollTemplateId)
--           Nothing ->
--             let rule = rulesMap Map.! rid
--             in  (rid, rule.receiverId, rule.pollTemplateId)
--
--     sendMessages :: [(ReceiverId, PollTemplateId)] -> IO ()
--     sendMessages candidates = do
--       receivers <- getReceiversWithSendersByIds receiverIds
--       templates <- IntMap.fromList . V.toList . V.map (\p -> (idToInt p.id, p)) <$> getPollTemplatesByIds pollTemplateIds
--       forM_ candidates $ \(rid, tid) -> sendMessage (receivers IntMap.! rid) (templates IntMap.! tid)
--       where
--         receiverIds = V.fromList $ IntSet.toList (IntSet.fromList (map fst candidates))
--         pollTemplateIds = V.map Id $ V.fromList $ IntSet.toList (IntSet.fromList (map snd candidates))
--
--     sendMessage :: (Receiver, Sender) -> Core.PollTemplateFull -> IO ()
--     sendMessage (receiver, sender) pollTemplate = do
--       -- TODO: determine sender (bot) id while saving it
--       let vkClient = mkDefaultClient (sender.accessToken)
--       ownerId <- traverse (getBotId . mkDefaultClient) (sender.botAccessToken)
--       (pollOwnerId, pollId) <- pollsCreate vkClient pollTemplate (negate <$> ownerId)
--       let client = maybe vkClient mkDefaultClient (sender.botAccessToken)
--       messagesSendMessage
--         client
--         (receiver ^. #peerId)
--         ("poll" <> T.pack (show pollOwnerId) <> "_" <> T.pack (show pollId))
--
-- pollsCreate :: VKClient -> Core.PollTemplateFull -> Maybe Int -> IO (Int, Int)
-- pollsCreate client pollTemplate ownerId = do
--   -- curTime <- getCurrentTime
--   -- let pollTitle = formatTime defaultTimeLocale "" curTime
--   sendMethod @(WithResponse VKPoll)
--     client
--     "polls.create"
--     ( patchParamsWithEndDate . patchParamsWithOwnerId $
--         [ mkPair "question" pollTemplate.question
--         , mkPair "is_anonymous" pollTemplate.isAnonymous
--         , mkPair "is_multiple" pollTemplate.isMultiple
--         , mkPair "add_answers" $ norm (V.map (\x -> x.value) pollTemplate.options)
--         ]
--     )
--     >>= \case
--       Left _ -> error "error while execution of polls.create method"
--       Right (WithResponse poll) -> pure (poll._ownerId, poll._id)
--   where
--     patchParamsWithOwnerId :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
--     patchParamsWithOwnerId lst = case ownerId of
--       Just i -> mkPair "owner_id" i : lst
--       Nothing -> lst
--
--     patchParamsWithEndDate :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
--     patchParamsWithEndDate lst = case pollTemplate.endsAt of
--       Just t -> mkPair "end_date" (double2Int (realToFrac (utcTimeToPOSIXSeconds t))) : lst
--       Nothing -> lst
--
--     norm :: V.Vector Core.PollTemplateOption -> T.Text
--     norm v = T.decodeUtf8 $ BS.toStrict $ J.encode v
--
-- messagesSendMessage :: VKClient -> Int -> T.Text -> IO ()
-- messagesSendMessage client peerId attachments = do
--   g <- newStdGen
--   let (randomId :: Int, _) = randomR (1, maxBound) g
--   sendMethod @(WithResponse ())
--     client
--     "messages.send"
--     [ mkPair "peer_id" peerId
--     , mkPair "attachment" attachments
--     , mkPair "random_id" randomId
--     ]
--     >>= \case
--       Left _ -> error "error while execution of messages.send method"
--       Right _ -> pure ()

initScheduler :: (WithDb, WithScheduler) => IO ()
initScheduler = do
  undefined

-- rules <- getAllRules
-- exceptions <- getAllExceptionRules
-- V.forM_ rules $ \(rid, rule) -> do
--   withScheduler $ \sch -> addRegularRule sch rid rule
-- V.forM_ exceptions $ \(rid, rule) -> do
--   withScheduler $ \sch -> addExceptionRule sch rid rule
