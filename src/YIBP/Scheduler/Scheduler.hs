{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module YIBP.Scheduler.Scheduler
  ( Scheduler
  , WithScheduler
  , withScheduler
  , mkScheduler
  , runScheduler
  , addRegularRule
  , editRegularRule
  , removeRegularRule
  , addExceptionRule
  , editExceptionRule
  , removeExceptionRule
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
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar

import Control.Monad.IO.Unlift

import YIBP.Core.Poll qualified as Core
import YIBP.Core.Rule
import YIBP.Scheduler.Util

import Control.Applicative
import Control.Monad
import GHC.Float.RealFracMethods
import GHC.Generics
import System.Timeout

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class

import Optics

import YIBP.Core.Receiver
import YIBP.Core.Sender
import YIBP.Db.PollTemplate
import YIBP.Db.Receiver
import YIBP.Db.Rule
import YIBP.VK.Client
import YIBP.VK.Types
import YIBP.Db.Db

type RuleId = Int
type ExceptionRuleId = Int
type ReceiverId = Int
type PollTemplateId = Int

data SchedulerRuleEvent
  = AddRegularRuleEvent RuleId RegularRule
  | EditRegularRuleEvent RuleId RegularRule
  | RemoveRegularRuleEvent RuleId
  | AddExceptionRuleEvent ExceptionRuleId ExceptionRule
  | EditExceptionRuleEvent ExceptionRuleId ExceptionRule
  | RemoveExceptionRuleEvent ExceptionRuleId

newtype Scheduler = Scheduler (TQueue SchedulerRuleEvent)

type WithScheduler = (?scheduler :: Scheduler)

withScheduler :: (WithScheduler) => (Scheduler -> m a) -> m a
withScheduler f = f ?scheduler

mkScheduler :: IO Scheduler
mkScheduler = Scheduler <$> newTQueueIO

addRegularRule :: Scheduler -> RuleId -> RegularRule -> IO ()
addRegularRule (Scheduler q) rid rule = atomically $ do
  writeTQueue q (AddRegularRuleEvent rid rule)

editRegularRule :: Scheduler -> RuleId -> RegularRule -> IO ()
editRegularRule (Scheduler q) rid rule = atomically $ do
  writeTQueue q (EditRegularRuleEvent rid rule)

removeRegularRule :: Scheduler -> RuleId -> IO ()
removeRegularRule (Scheduler q) rid = atomically $ do
  writeTQueue q (RemoveRegularRuleEvent rid)

addExceptionRule :: Scheduler -> ExceptionRuleId -> ExceptionRule -> IO ()
addExceptionRule (Scheduler q) rid rule = atomically $ do
  writeTQueue q (AddExceptionRuleEvent rid rule)

editExceptionRule :: Scheduler -> ExceptionRuleId -> ExceptionRule -> IO ()
editExceptionRule (Scheduler q) rid rule = atomically $ do
  writeTQueue q (EditExceptionRuleEvent rid rule)

removeExceptionRule :: Scheduler -> ExceptionRuleId -> IO ()
removeExceptionRule (Scheduler q) rid = atomically $ do
  writeTQueue q (RemoveExceptionRuleEvent rid)

doSTMWithTimeout :: Int -> STM a -> IO (Maybe a)
doSTMWithTimeout n action = do
  delay <- registerDelay n
  atomically $
    Just <$> action
      <|> Nothing <$ (readTVar delay >>= check)

runScheduler :: (WithDb, WithScheduler) => IO ()
runScheduler = withScheduler $ \(Scheduler queue) ->
  runScheduler' queue Map.empty Map.empty Map.empty

runScheduler'
  :: (WithDb)
  => TQueue SchedulerRuleEvent
  -> Map RuleId UTCTime
  -> Map RuleId RegularRule
  -> Map ExceptionRuleId ExceptionRule
  -> IO ()
runScheduler' queue = loop
  where
    loop nextTimeMap rulesMap exceptionsMap = do
      curTime <- getCurrentTime
      let delay = getNextDelay curTime nextTimeMap
      doSTMWithTimeout delay (readTQueue queue) >>= \case
        Nothing -> do
          candidates <- (\t -> getCandidates t nextTimeMap rulesMap exceptionsMap) <$> getCurrentTime
          _ <- forkIO $ sendMessages $ map (\(_, a, b) -> (a, b)) candidates
          let ruleIds = Set.fromList $ map (\(a, _, _) -> a) candidates
          tz <- getCurrentTimeZone
          let nextTimeMap' =
                Map.mapWithKey
                  ( \rid t ->
                      if Set.member rid ruleIds
                        then getNextTime ((rulesMap Map.! rid) ^. #cronRule) t tz
                        else t
                  )
                  nextTimeMap
          loop nextTimeMap' rulesMap exceptionsMap
        Just (AddRegularRuleEvent rid rule) -> do
          timeNow <- getCurrentTime
          tz <- getCurrentTimeZone
          loop
            (Map.insert rid (getNextTime (rule ^. #cronRule) timeNow tz) nextTimeMap)
            (Map.insert rid rule rulesMap)
            exceptionsMap
        Just (EditRegularRuleEvent rid rule) -> do
          timeNow <- getCurrentTime
          tz <- getCurrentTimeZone
          loop
            (Map.insert rid (getNextTime (rule ^. #cronRule) timeNow tz) nextTimeMap)
            (Map.insert rid rule rulesMap)
            exceptionsMap
        Just (RemoveRegularRuleEvent rid) ->
          loop (Map.delete rid nextTimeMap) (Map.delete rid rulesMap) exceptionsMap
        Just (AddExceptionRuleEvent rid rule) ->
          loop nextTimeMap rulesMap (Map.insert rid rule exceptionsMap)
        Just (EditExceptionRuleEvent rid rule) ->
          loop nextTimeMap rulesMap (Map.insert rid rule exceptionsMap)
        Just (RemoveExceptionRuleEvent rid) ->
          loop nextTimeMap rulesMap (Map.delete rid exceptionsMap)

    getNextTime :: MyCronSchedule -> UTCTime -> TimeZone -> UTCTime
    getNextTime cron t tz = case nextMatch cron (localTimeToUTC utc (utcToLocalTime tz t)) of
      Just time -> localTimeToUTC tz (utcToLocalTime utc time)
      Nothing -> addUTCTime (1000 * nominalDay) t -- add sufficient delay
    getNextDelay :: UTCTime -> Map RuleId UTCTime -> RuleId
    getNextDelay curTime m = case Set.lookupMin (Set.fromList (Map.elems m)) of
      Just t ->
        let diffTime = diffUTCTime t curTime
        in  if diffTime < 0
              then 0
              else floorDoubleInt $ realToFrac diffTime * 1000000
      Nothing -> maxBound

    getCandidates
      :: UTCTime
      -> Map RuleId UTCTime
      -> Map RuleId RegularRule
      -> Map ExceptionRuleId ExceptionRule
      -> [(RuleId, ReceiverId, PollTemplateId)]
    getCandidates curTime m rulesMap exceptionsMap = map processElement candidates
      where
        candidates = Map.assocs $ Map.filter (< curTime) m

        tryFindException :: RuleId -> UTCTime -> Maybe ExceptionRule
        tryFindException rid t =
          find (\er -> (er ^. #regularRuleId == rid) && (er ^. #sendAt == t)) (Map.elems exceptionsMap)

        processElement :: (RuleId, UTCTime) -> (RuleId, ReceiverId, PollTemplateId)
        processElement (rid, t) = case tryFindException rid t of
          Just er -> (rid, er ^. #receiverId, er ^. #pollTemplateId)
          Nothing ->
            let rule = rulesMap Map.! rid
            in  (rid, rule ^. #receiverId, rule ^. #pollTemplateId)

    sendMessages :: [(ReceiverId, PollTemplateId)] -> IO ()
    sendMessages candidates = do
      receivers <- getReceiversWithSendersByIds receiverIds
      templates <- getPollTemplatesByIds pollTemplateIds
      forM_ candidates $ \(rid, tid) -> sendMessage (receivers IntMap.! rid) (templates IntMap.! tid)
      where
        receiverIds = V.fromList $ IntSet.toList (IntSet.fromList (map fst candidates))
        pollTemplateIds = V.fromList $ IntSet.toList (IntSet.fromList (map snd candidates))

    sendMessage :: (Receiver, Sender) -> Core.PollTemplate -> IO ()
    sendMessage (receiver, sender) pollTemplate = do
      -- TODO: determine sender (bot) id while saving it
      let vkClient = mkDefaultClient (sender ^. #accessToken)
      ownerId <- traverse (getBotId . mkDefaultClient) (sender ^. #botAccessToken)
      (pollOwnerId, pollId) <- pollsCreate vkClient pollTemplate (negate <$> ownerId)
      let client = maybe vkClient mkDefaultClient (sender ^. #botAccessToken)
      messagesSendMessage
        client
        (receiver ^. #peerId)
        ("poll" <> T.pack (show pollOwnerId) <> "_" <> T.pack (show pollId))

getBotId :: VKClient -> IO Int
getBotId client =
  sendMethod @(WithResponse (V.Vector VKGroupFull)) client "groups.getById" [] >>= \case
    Left _ -> error "error while execution of groups.getById method" -- TODO: make better error handling
    Right (WithResponse x) -> case x V.!? 0 of
      Just r -> pure $ r ^. #_id
      Nothing -> error "error while execution of groups.getById method"

pollsCreate :: VKClient -> Core.PollTemplate -> Maybe Int -> IO (Int, Int)
pollsCreate client pollTemplate ownerId = do
  -- curTime <- getCurrentTime
  -- let pollTitle = formatTime defaultTimeLocale "" curTime
  sendMethod @(WithResponse VKPoll)
    client
    "polls.create"
    ( patchParamsWithEndDate . patchParamsWithOwnerId $
        [ mkPair "question" ("Poll" :: T.Text)
        , mkPair "is_anonymous" (pollTemplate ^. #isAnonymous)
        , mkPair "is_multiple" (pollTemplate ^. #isMultiple)
        , mkPair "add_answers" $ norm (pollTemplate ^. #options)
        ]
    )
    >>= \case
      Left _ -> error "error while execution of polls.create method"
      Right (WithResponse poll) -> pure (poll ^. #_ownerId, poll ^. #_id)
  where
    patchParamsWithOwnerId :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
    patchParamsWithOwnerId lst = case ownerId of
      Just i -> mkPair "owner_id" i : lst
      Nothing -> lst

    patchParamsWithEndDate :: [(T.Text, T.Text)] -> [(T.Text, T.Text)]
    patchParamsWithEndDate lst = case pollTemplate ^. #endsAt of
      Just t -> mkPair "end_date" (double2Int (realToFrac (utcTimeToPOSIXSeconds t))) : lst
      Nothing -> lst

    norm :: V.Vector T.Text -> T.Text
    norm v = T.decodeUtf8 $ BS.toStrict $ J.encode v

messagesSendMessage :: VKClient -> Int -> T.Text -> IO ()
messagesSendMessage client peerId attachments = do
  g <- newStdGen
  let (randomId :: Int, _) = randomR (1, maxBound) g
  sendMethod @(WithResponse ())
    client
    "messages.send"
    [ mkPair "peer_id" peerId
    , mkPair "attachment" attachments
    , mkPair "random_id" randomId
    ]
    >>= \case
      Left _ -> error "error while execution of messages.send method"
      Right _ -> pure ()

initScheduler :: (WithDb, WithScheduler) => IO ()
initScheduler = do
  rules <- getAllRules
  exceptions <- getAllExceptionRules
  V.forM_ rules $ \(rid, rule) -> do
    withScheduler $ \sch -> addRegularRule sch rid rule
  V.forM_ exceptions $ \(rid, rule) -> do
    withScheduler $ \sch -> addExceptionRule sch rid rule