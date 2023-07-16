{-# LANGUAGE OverloadedStrings #-}

module YIBP.Scheduler.Scheduler
  ( Scheduler
  , mkScheduler
  , runScheduler
  , initScheduler
  ) where

import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (find)
import Data.Maybe (catMaybes, isNothing)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Vector qualified as V

import System.IO.Unsafe
import System.Random

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, catch, throwIO)
import Control.Monad

import YIBP.Scheduler
import YIBP.Scheduler.Util

import YIBP.Core.Id
import YIBP.Core.PollTemplate
import YIBP.Core.Rule
import YIBP.Core.Sender
import YIBP.Crypto
import YIBP.Db
import YIBP.Service.Rule qualified as Service
import YIBP.VK.Client ((=--=))
import YIBP.VK.Client qualified as VK
import YIBP.VK.Types

import Fmt

import GHC.Float.RealFracMethods

mkScheduler :: IO Scheduler
mkScheduler = Scheduler <$> newTQueueIO

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
      putStrLn $ "Sleeping for " <> show delay <> " us [current time = " <> show curTime <> "]"
      doSTMWithTimeout delay (readTQueue queue) >>= \case
        Nothing -> do
          state' <- onExecRule state
          curTime' <- getCurrentTime
          tz <- getCurrentTimeZone
          let (_, state'') = updateNextExecTime curTime' tz (IntMap.keysSet state'.nextTime) state'
          loop state''
        Just ev -> do
          putStrLn $ "Got new event: " <> show ev
          onRuleEvent ev state >>= loop

data Candidate = Candidate
  { ruleId :: !Int
  , senderId :: !(Id SenderTag)
  , peerId :: !Int
  , pollTemplateId :: !(Id PollTemplate)
  }
  deriving (Show)

data CandidateFull = CandidateFull
  { sender :: !(Sender 'Decrypted)
  , peerId :: !Int
  , pollTemplate :: !PollTemplate
  }
  deriving (Show)

getFullCandidate :: (WithDb) => Candidate -> IO (Maybe CandidateFull)
getFullCandidate c = do
  fmap tr <$> Service.getDetailedRegularRule (c.senderId, c.pollTemplateId)
  where
    tr (s, pt) = CandidateFull {sender = s, peerId = c.peerId, pollTemplate = pt}

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
    processRule (rId, r) =
      case findReplaceRule rId of
        Just replaceRule ->
          Candidate
            { ruleId = rId
            , senderId = r.senderId
            , peerId = r.peerId
            , pollTemplateId = replaceRule.newPollTemplateId
            }
        _ ->
          Candidate
            { ruleId = rId
            , senderId = r.senderId
            , peerId = r.peerId
            , pollTemplateId = r.pollTemplateId
            }
    findReplaceRule :: Int -> Maybe ReplaceRule
    findReplaceRule rId =
      snd
        <$> find
          ( \(_, rule) ->
              (idToInt rule.regularRuleId == rId) && (rule.sendAt == (state.nextTime IntMap.! rId))
          )
          (IntMap.assocs state.replaceRules)

newtype SendMessageError = SendMessageError T.Text
  deriving (Show, Eq)
  deriving anyclass (Exception)

createPoll :: VK.VKClient -> PollTemplate -> Maybe Int -> IO (Maybe (Int, Int))
createPoll client tmpl groupId = do
  let options = V.map getPollTemplateOption tmpl.options
  let params =
        catMaybes
          [ Just $ "question" =--= tmpl.question
          , Just $ "is_anonymous" =--= tmpl.isAnonymous
          , Just $ "is_multiple" =--= tmpl.isMultiple
          , Just $ "add_answers" =--= T.decodeUtf8 (BS.toStrict $ J.encode options)
          , ("owner_id" =--=) <$> groupId
          , ("end_date" =--=) <$> tmpl.endsAt
          ]
  VK.sendMethod @(VK.WithResponse VKPoll) client "polls.create" params >>= \case
    Left _ -> pure Nothing
    Right (VK.WithResponse poll) -> pure $ Just (poll.ownerId, poll.id)

sendMessage :: CandidateFull -> IO ()
sendMessage c = do
  let userClient = VK.mkDefaultClient c.sender.accessToken
  let senderClient = VK.mkDefaultClient (getSenderToken c.sender)
  (ownerId, pollId) <-
    createPoll userClient c.pollTemplate ((.id) <$> c.sender.bot) >>= \case
      Nothing -> throwIO $ SendMessageError "unable to create poll template"
      Just p -> pure p
  let attachment :: T.Text = "poll" +| ownerId |+ "_" +| pollId |+ ""
  randomId :: Int <- fst . randomR (1, maxBound) <$> newStdGen
  VK.sendMethod @(VK.WithResponse ())
    senderClient
    "messages.send"
    [ "peer_id" =--= c.peerId
    , "attachment" =--= attachment
    , "random_id" =--= randomId
    ]
    >>= \case
      Left _ -> throwIO $ SendMessageError "unable to send message"
      Right _ -> pure ()

getObsoleteRules :: UTCTime -> TimeZone -> SchedulerState -> IntSet
getObsoleteRules curTime tz state =
  obsoleteRegularRules `IntSet.union` obsoleteIgnoreRules `IntSet.union` obsoleteReplaceRules
  where
    obsoleteRegularRules =
      IntMap.keysSet $
        IntMap.filterWithKey (\_ v -> isNothing (calculateNextExecTime v.schedule curTime tz)) state.regularRules
    obsoleteIgnoreRules =
      IntMap.keysSet $
        IntMap.filterWithKey (\_ v -> v.sendAt <= curTime) state.ignoreRules
    obsoleteReplaceRules =
      IntMap.keysSet $
        IntMap.filterWithKey (\_ v -> v.sendAt <= curTime) state.replaceRules

updateNextExecTime :: UTCTime -> TimeZone -> IntSet -> SchedulerState -> (IntSet, SchedulerState)
updateNextExecTime curTime tz idsToUpdate state = IntMap.foldlWithKey' go (IntSet.empty, state) state.nextTime
  where
    sc i = (state.regularRules IntMap.! i).schedule
    go (obsolete, s) i _
      | i `IntSet.member` idsToUpdate = case calculateNextExecTime (sc i) curTime tz of
          Just t -> (obsolete, s {nextTime = IntMap.insert i t s.nextTime})
          Nothing -> (IntSet.insert i obsolete, s {nextTime = IntMap.delete i s.nextTime, regularRules = IntMap.delete i s.regularRules})
      | otherwise = (obsolete, s)

onExecRule :: (WithDb, WithScheduler) => SchedulerState -> IO SchedulerState
onExecRule state = do
  curTime <- getCurrentTime
  tz <- getCurrentTimeZone
  let candidates = getExecCandidates curTime state
  putStrLn $ "Candidates: " <> show candidates
  fullCandidates <- V.forM candidates getFullCandidate

  V.forM_ (V.catMaybes fullCandidates) $ \c -> do
    putStrLn $ "Sending message: " <> show c
    sendMessage c
      `catch` (\(e :: SomeException) -> putStrLn $ "Failed to send message of candidate " <> show c <> " with error: " <> show e)

  let idsToUpdate = IntSet.fromList $ V.toList $ V.map (.ruleId) candidates
  let obsoleteRules = getObsoleteRules curTime tz state
  let (obsoleteRegularRules, state') = updateNextExecTime curTime tz idsToUpdate state
  forM_ (map Id (IntSet.toList (obsoleteRules `IntSet.union` obsoleteRegularRules))) removeRule
  -- set can_trigger = false
  pure state'

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

initScheduler :: (WithDb, WithScheduler) => IO ()
initScheduler = do
  rules <- Service.getAllActiveRules
  V.forM_ rules $ \(rId, rule) -> do
    addRule rId rule
