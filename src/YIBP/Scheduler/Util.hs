module YIBP.Scheduler.Util where
import System.Cron qualified as Cron
import System.Cron.Parser qualified as Cron
import System.Cron.Types qualified as Cron

import Data.Aeson
import Data.Text qualified as T
import Data.Time

newtype MyCronSchedule = MyCronSchedule Cron.CronSchedule
  deriving (Show, Eq)

instance FromJSON MyCronSchedule where
  parseJSON = withText "MyCronSchedule" $ \t ->
    case parseMyCronSchedule t of
      Left err -> fail err
      Right x -> pure x

instance ToJSON MyCronSchedule where
  toJSON (MyCronSchedule x) = String (Cron.serializeCronSchedule x)

parseMyCronSchedule :: T.Text -> Either String MyCronSchedule
parseMyCronSchedule t = MyCronSchedule <$> Cron.parseCronSchedule t

nextMatch :: MyCronSchedule -> UTCTime -> Maybe UTCTime
nextMatch (MyCronSchedule s) = Cron.nextMatch s

scheduleMatches :: MyCronSchedule -> UTCTime -> Bool
scheduleMatches (MyCronSchedule s) = Cron.scheduleMatches s

serializeCronSchedule :: MyCronSchedule -> T.Text
serializeCronSchedule (MyCronSchedule x) = Cron.serializeCronSchedule x