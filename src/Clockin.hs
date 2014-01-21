{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Track clocking in and out of work.

module Clockin where

import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Conduit
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as CL
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Data.Time.Lens
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           System.Locale
import           Text.Printf

-- | Configuration for the clocking in setup.
data Config = Config
  { configFilePath    :: !FilePath
  , configHoursPerDay :: Int
  } deriving (Show)

-- | An entry in the clocking log.
data Entry
  = In !ClockIn
  | Out !ClockOut
  deriving (Show,Generic)

instance ToJSON Entry

instance FromJSON Entry

-- | Clocking in.
data ClockIn = ClockIn
  { inProject :: !Text
  , inTask    :: !(Maybe Text)
  , inTime    :: !UTCTime
  } deriving (Show,Generic)

instance ToJSON ClockIn

instance FromJSON ClockIn

-- | Clocking out.
data ClockOut = ClockOut
  { outProject :: !Text
  , outTask    :: !(Maybe Text)
  , outReason  :: !(Maybe Text)
  , outTime    :: !UTCTime
  } deriving (Show,Generic)

instance ToJSON ClockOut

instance FromJSON ClockOut

-- | A status report for the current log.
data Status = Status
  { statusIn             :: !Bool            -- ^ Am I clocked in?
  , statusCurTimePeriod  :: !NominalDiffTime -- ^ How long have I been clocked in/clocked out for?
  , statusInToday        :: !NominalDiffTime -- ^ How long have I been in today?
  , statusRemainingToday :: !NominalDiffTime -- ^ How long left today?
  } deriving (Show)

-- | Get a default config.
getClockinConfig :: IO Config
getClockinConfig =
  do dir <- getHomeDirectory
     return (Config (dir </> ".clockin.log")
                    8)

-- | Clock into something.
clockIn :: Config     -- ^ Config.
        -> Text       -- ^ Project.
        -> Maybe Text -- ^ Task.
        -> IO ()
clockIn config project task =
  do now <- getCurrentTime
     clock config (In (ClockIn project task now))

-- | Clock out of something.
clockOut :: Config    -- ^ Config.
        -> Text       -- ^ Project.
        -> Maybe Text -- ^ Task.
        -> Maybe Text -- ^ Reason.
        -> IO ()
clockOut config project task reason =
  do now <- getCurrentTime
     clock config (Out (ClockOut project task reason now))

-- | Clock in or out.
clock :: Config -> Entry -> IO ()
clock config entry =
  L.appendFile (configFilePath config)
               (encode entry <> "\n")

-- | Print out a status string.
printClockinStatus :: Config -> IO ()
printClockinStatus config =
  do entries <- readClockinEntries config
     now <- getZonedTime
     T.putStrLn (describeStatus now (clockinStatus config (zonedTimeToUTC now) entries))

-- | Read in the log entries from file.
readClockinEntries :: Config -> IO [Entry]
readClockinEntries config =
  runResourceT (C.sourceFile (configFilePath config) $=
                C.lines $=
                CL.mapMaybe (decode . L.fromStrict) $$
                CL.consume)

-- | Make a human-readable representation of the status.
describeStatus :: ZonedTime -> Status -> Text
describeStatus now status =
  T.unlines ["Current time is: " <> T.pack (formatTime defaultTimeLocale "%F %R" now)
            ,"Currently clocked " <> (if statusIn status
                                         then "IN "
                                         else "OUT ")
                                  <> (if statusCurTimePeriod status == 0 && not (statusIn status)
                                         then ""
                                         else "(" <>
                                              diffTime (-1 * (statusCurTimePeriod status)) True <>
                                              ")")
            ,"Time spent today: " <> hours (-1 * (statusInToday status))
            ,"Remaining: " <> hours (statusRemainingToday status)
            ]
  where hours = T.pack .
                formatTime defaultTimeLocale "%R" .
                (`addUTCTime` startOfDay (zonedTimeToUTC now))

-- | Make a short human-readable representation of the status, on one line.
onelinerStatus :: ZonedTime -> Status -> Text
onelinerStatus now status =
  "Worked/remaining: " <>
  hours (-1 * (statusInToday status)) <>
  "/" <>
  hours (statusRemainingToday status) <>
  " (clocked " <>
  (if statusIn status then "in" else "out") <>
  ")"
  where hours = T.pack .
                formatTime defaultTimeLocale "%R" .
                (`addUTCTime` startOfDay (zonedTimeToUTC now))

-- | Generate a status report of the current log.
clockinStatus :: Config -> UTCTime -> [Entry] -> Status
clockinStatus config now entries =
  Status clockedIn
         curPeriod
         todayDiff
         remaining
  where remaining =
          diffUTCTime (addUTCTime (60 * 60 * fromIntegral (configHoursPerDay config))
                                  midnight)
                      (addUTCTime (-1 * todayDiff)
                                  midnight)
        tomorrow = modL day (+1) now
        todayDiff = inToday now descending
        clockedIn =
          fromMaybe False
                    (fmap (\i -> case i of
                                   In{} -> True
                                   _ -> False)
                          current)
        curPeriod = maybe 0 (diffUTCTime now . entryTime) current
        current = listToMaybe descending
        descending = reverse entries
        midnight = startOfDay now

-- | Get the time, if any, of an entry's clocking in.
entryTime :: Entry -> UTCTime
entryTime (In i) = inTime i
entryTime (Out o) = outTime o

-- | Get the starting time of the day of the given time.
startOfDay :: UTCTime -> UTCTime
startOfDay time =
  case time of
    UTCTime day _ -> UTCTime day 0

-- | How much time clocked in today? Expects a DESCENDING entry list.
--
-- If the clocking in time was yesterday, then don't include the work
-- from yesterday, only include the work for today starting from
-- midnight.
--
-- Stops traversing the list if it reaches an entry from yesterday.
inToday :: UTCTime -> [Entry] -> NominalDiffTime
inToday now = go now 0 . zip [0..]
  where go last total (((i,x):xs)) =
          case x of
            In{} | today -> go this (total + diffUTCTime this last) xs
                 | otherwise -> go this (total + diffUTCTime (startOfDay now) last) []
            Out{} | today -> go this total xs
                  | i == 0 -> go this total []
                  | otherwise -> go this total []
          where this = entryTime x
                today = utctDay this == utctDay now
        go _ total _ = total

-- | Display a time span as one time relative to another.
diffTime :: NominalDiffTime
         -> Bool    -- ^ Display 'in/ago'?
         -> Text   -- ^ Example: '3 seconds ago', 'in three days'.
diffTime span' fix = T.pack $ maybe "unknown" format $ find (\(s,_,_) -> abs span'>=s) $ reverse ranges where
  minute = 60; hour = minute * 60; day = hour * 24;
  week = day * 7; month = day * 30; year = month * 12
  format range =
    (if fix && span'>0 then "in " else "")
    ++ case range of
        (_,str,0) -> str
        (_,str,base) -> printf str (abs $ round (span' / base) :: Integer)
    ++ (if fix && span'<0 then " ago" else "")
  ranges = [(0,"%d seconds",1)
           ,(minute,"a minute",0)
           ,(minute*2,"%d minutes",minute)
           ,(minute*30,"half an hour",0)
           ,(minute*31,"%d minutes",minute)
           ,(hour,"an hour",0)
           ,(hour*2,"%d hours",hour)
           ,(hour*3,"a few hours",0)
           ,(hour*4,"%d hours",hour)
           ,(day,"a day",0)
           ,(day*2,"%d days",day)
           ,(week,"a week",0)
           ,(week*2,"%d weeks",week)
           ,(month,"a month",0)
           ,(month*2,"%d months",month)
           ,(year,"a year",0)
           ,(year*2,"%d years",year)
           ]
