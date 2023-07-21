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
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Vector qualified as V

import System.Random

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, catch, throwIO)
import Control.Monad

import YIBP.Scheduler
import YIBP.Scheduler.Util

import YIBP.Logger
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

import Data.Maybe
import GHC.Float.RealFracMethods

mkScheduler :: IO Scheduler
mkScheduler = Scheduler <$> newTQueueIO

doSTMWithTimeout :: Int -> STM a -> IO (Maybe a)
doSTMWithTimeout n action = do
  delay <- registerDelay n
  atomically $
    Just <$> action
      <|> Nothing <$ (readTVar delay >>= check)

data RegularRuleContext = RegularRuleContext
  { nextTime :: !UTCTime
  , rule :: !RegularRule
  , ignoreRuleIds :: !IntSet
  , replaceRuleIds :: !IntSet
  }
  deriving (Show)

emptyRegularRuleContext :: UTCTime -> RegularRule -> RegularRuleContext
emptyRegularRuleContext t r = RegularRuleContext t r IntSet.empty IntSet.empty

data RuleTag = ReplaceRuleTag | IgnoreRuleTag

modifyRegularRuleContext :: RuleTag -> (IntSet -> IntSet) -> RegularRuleContext -> RegularRuleContext
modifyRegularRuleContext ReplaceRuleTag f ctx = ctx {replaceRuleIds = f ctx.replaceRuleIds}
modifyRegularRuleContext IgnoreRuleTag f ctx = ctx {ignoreRuleIds = f ctx.ignoreRuleIds}

regularRuleShouldBeIgnored :: SchedulerState -> RegularRuleContext -> Bool
regularRuleShouldBeIgnored state ctx =
  any (\r -> r.sendAt == ctx.nextTime) $
    ctx.ignoreRuleIds
      & IntSet.toList
      & mapMaybe (`IntMap.lookup` state.ignoreRules)

data SchedulerState = SchedulerState
  { regularRules :: !(IntMap RegularRuleContext)
  , ignoreRules :: !(IntMap IgnoreRule)
  , replaceRules :: !(IntMap ReplaceRule)
  , tz :: !TimeZone
  }

addRuleS :: UTCTime -> SchedulerState -> Int -> Rule -> SchedulerState
addRuleS curTime state rId (Rule (RMRegular rule) _) =
  case calculateNextExecTime rule.schedule curTime state.tz of
    Just t ->
      let ignoreRuleIds = IntMap.keysSet $ IntMap.filter (\r -> idToInt r.regularRuleId == rId) state.ignoreRules
          replaceRuleIds = IntMap.keysSet $ IntMap.filter (\r -> idToInt r.regularRuleId == rId) state.replaceRules
          ctx = RegularRuleContext {nextTime = t, rule = rule, ignoreRuleIds, replaceRuleIds}
      in  state {regularRules = IntMap.insert rId ctx state.regularRules}
    Nothing -> state
addRuleS _ state rId (Rule (RMIgnore rule) _) =
  case IntMap.lookup regularRuleId state.regularRules of
    Nothing -> state
    Just _ ->
      state
        { regularRules = IntMap.adjust (modifyRegularRuleContext IgnoreRuleTag (IntSet.insert rId)) regularRuleId state.regularRules
        , ignoreRules = IntMap.insert rId rule state.ignoreRules
        }
  where
    regularRuleId = idToInt rule.regularRuleId
addRuleS _ state rId (Rule (RMReplace rule) _) =
  case IntMap.lookup regularRuleId state.regularRules of
    Nothing -> state
    Just _ ->
      state
        { regularRules = IntMap.adjust (modifyRegularRuleContext ReplaceRuleTag (IntSet.insert rId)) regularRuleId state.regularRules
        , replaceRules = IntMap.insert rId rule state.replaceRules
        }
  where
    regularRuleId = idToInt rule.regularRuleId

deleteRuleS :: SchedulerState -> Int -> SchedulerState
deleteRuleS state rId
  | IntMap.member rId state.regularRules =
      case IntMap.lookup rId state.regularRules of
        Nothing -> state
        Just ctx ->
          state
            { regularRules = IntMap.delete rId state.regularRules
            , ignoreRules = IntMap.withoutKeys state.ignoreRules ctx.ignoreRuleIds
            , replaceRules = IntMap.withoutKeys state.replaceRules ctx.replaceRuleIds
            }
  | IntMap.member rId state.ignoreRules =
      case IntMap.lookup rId state.ignoreRules of
        Nothing -> state
        Just ir ->
          let regularRuleId = idToInt ir.regularRuleId
          in  state
                { regularRules = IntMap.adjust (modifyRegularRuleContext IgnoreRuleTag (IntSet.delete rId)) regularRuleId state.regularRules
                , ignoreRules = IntMap.delete rId state.ignoreRules
                }
  | IntMap.member rId state.replaceRules =
      case IntMap.lookup rId state.replaceRules of
        Nothing -> state
        Just rr ->
          let regularRuleId = idToInt rr.regularRuleId
          in  state
                { regularRules = IntMap.adjust (modifyRegularRuleContext ReplaceRuleTag (IntSet.delete rId)) regularRuleId state.regularRules
                , replaceRules = IntMap.delete rId state.replaceRules
                }
  | otherwise = state

runScheduler :: (WithDb, WithScheduler, WithLogger) => IO ()
runScheduler = do
  tz <- getCurrentTimeZone
  runScheduler' (SchedulerState IntMap.empty IntMap.empty IntMap.empty tz)

runScheduler'
  :: (WithDb, WithScheduler, WithLogger)
  => SchedulerState
  -> IO ()
runScheduler' = loop
  where
    getDelay state = do
      curTime <- getCurrentTime
      let delta = (`diffUTCTime` curTime) <$> getNearestEventTime state
      let delay = floorDoubleInt . (* 1_000_000) . realToFrac <$> delta
      pure $ max 0 (fromMaybe maxBound delay)
    Scheduler queue = withScheduler id
    loop state = do
      delay <- getDelay state
      logMsg Debug $ "Sleeping for " +| show delay |+ " us"
      s' <-
        doSTMWithTimeout delay (readTQueue queue) >>= \case
          Nothing -> do
            curTime <- getCurrentTime
            let rulesToBeUpdated = IntMap.filter (\r -> r.nextTime <= curTime) state.regularRules
            let rulesToBeExecuted = [(i, r) | (i, r) <- IntMap.assocs rulesToBeUpdated, not (regularRuleShouldBeIgnored state r)]
            _ <- forkIO $ forM_ rulesToBeExecuted $ \(i, r) -> do
              execRule state i r
                `catch` (\(e :: SomeException) -> logMsg Error $ "Failed to send message according to rule" +| show r |+ " with error: " +| show e |+ "")
            let rulesToDelete = getRuleIdsToDelete curTime state
            Service.markRulesObsolete (IntSet.toList rulesToDelete & map Id & V.fromList)
            let state' = IntSet.foldl' deleteRuleS state rulesToDelete
            pure $ updateNextTimes curTime state' rulesToBeUpdated
          Just ev -> do
            logMsg Debug $ "New event: " +| show ev |+ ""
            onRuleEvent ev state
      loop s'

getRuleIdsToDelete :: UTCTime -> SchedulerState -> IntSet
getRuleIdsToDelete curTime state =
  regularRuleIdsToDelete
    <> IntMap.keysSet (IntMap.filter (\x -> x.sendAt < curTime) state.ignoreRules)
    <> IntMap.keysSet (IntMap.filter (\x -> x.sendAt < curTime) state.replaceRules)
  where
    regularRuleIdsToDelete = IntMap.foldlWithKey' go IntSet.empty state.regularRules
      where
        go acc rId ctx = case calculateNextExecTime ctx.rule.schedule curTime state.tz of
          Nothing -> acc <> IntSet.singleton rId <> ctx.ignoreRuleIds <> ctx.replaceRuleIds
          Just _ -> acc

updateNextTimes :: UTCTime -> SchedulerState -> IntMap RegularRuleContext -> SchedulerState
updateNextTimes curTime = IntMap.foldlWithKey' go
  where
    go state regularRuleId ctx = case calculateNextExecTime ctx.rule.schedule curTime state.tz of
      Nothing -> state
      Just t -> state {regularRules = IntMap.adjust (\x -> x {nextTime = t}) regularRuleId state.regularRules}

getNearestEventTime :: SchedulerState -> Maybe UTCTime
getNearestEventTime state = IntMap.foldl' go Nothing state.regularRules
  where
    go :: Maybe UTCTime -> RegularRuleContext -> Maybe UTCTime
    go acc ctx = Just $ min (fromMaybe ctx.nextTime acc) ctx.nextTime

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

execRule :: (WithDb) => SchedulerState -> Int -> RegularRuleContext -> IO ()
execRule state rId ctx = do
  fullCandidate <-
    getFullCandidate candidate >>= \case
      Just t -> pure t
      Nothing -> throwIO $ SendMessageError "unable to fetch full candidate"
  sendMessage fullCandidate
  where
    defaultCandidate :: Candidate
    defaultCandidate =
      Candidate
        { ruleId = rId
        , senderId = ctx.rule.senderId
        , peerId = ctx.rule.peerId
        , pollTemplateId = ctx.rule.pollTemplateId
        }
    candidate :: Candidate
    candidate = case IntSet.maxView ctx.replaceRuleIds of
      Nothing -> defaultCandidate
      Just (replaceRuleId, _) -> case IntMap.lookup replaceRuleId state.replaceRules of
        Nothing -> defaultCandidate
        Just replaceRule ->
          Candidate
            { ruleId = rId
            , senderId = ctx.rule.senderId
            , peerId = ctx.rule.peerId
            , pollTemplateId = replaceRule.newPollTemplateId
            }

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

onRuleEvent :: SchedulerRuleEvent -> SchedulerState -> IO SchedulerState
onRuleEvent (AddRuleEvent rid rule) state = do
  curTime <- getCurrentTime
  pure $ addRuleS curTime state rid rule
onRuleEvent (EditRuleEvent rid rule) state =
  onRuleEvent (RemoveRuleEvent rid) state >>= onRuleEvent (AddRuleEvent rid rule)
onRuleEvent (RemoveRuleEvent rid) state = pure $ deleteRuleS state rid

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

initScheduler :: (WithDb, WithScheduler) => IO ()
initScheduler = do
  rules <- Service.getAllActiveRules
  V.forM_ rules $ \(rId, rule) -> do
    addRule rId rule
