{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Track clocking in and out of work.

module Clockin where

import           Control.Monad
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
import           Data.Time.Locale.Compat
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
  , inTime    :: !Time
  } deriving (Show,Generic)

instance ToJSON ClockIn

instance FromJSON ClockIn

-- | Clocking out.
data ClockOut = ClockOut
  { outProject :: !Text
  , outTask    :: !(Maybe Text)
  , outReason  :: !(Maybe Text)
  , outTime    :: !Time
  } deriving (Show,Generic)

instance ToJSON ClockOut

instance FromJSON ClockOut

-- | A status report for the current log.
data Status = Status
  { statusIn             :: !Bool            -- ^ Am I clocked in?
  , statusCurTimePeriod  :: !NominalDiffTime -- ^ How long have I been clocked in/clocked out for?
  , statusInToday        :: !NominalDiffTime -- ^ How long have I been in today?
  , statusRemainingToday :: !Remaining       -- ^ How long left today?
  } deriving (Show)

-- | How much time remaining.
data Remaining
  = Credit NominalDiffTime
  | Due NominalDiffTime
    deriving (Eq,Show)

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
  do now <- getLocalTime
     clock config (In (ClockIn project task (Time now)))

-- | Clock out of something.
clockOut :: Config    -- ^ Config.
        -> Text       -- ^ Project.
        -> Maybe Text -- ^ Task.
        -> Maybe Text -- ^ Reason.
        -> IO ()
clockOut config project task reason =
  do now <- getLocalTime
     clock config (Out (ClockOut project task reason (Time now)))

-- | Clock in or out.
clock :: Config -> Entry -> IO ()
clock config entry =
  L.appendFile (configFilePath config)
               (encode entry <> "\n")

-- | Print out a status string.
printClockinStatus :: Config -> IO ()
printClockinStatus config =
  do entries <- readClockinEntries config
     now <- getLocalTime
     T.putStrLn (describeStatus now (clockinStatus config now entries) entries)

-- | Print hours worked per day in a format spark can consume.
printSparkDays :: Config -> IO ()
printSparkDays config =
  do entries <- readClockinEntries config
     now <- getLocalTime
     forM_ (days now entries)
           (\day ->
              let diff =
                    -1 * (statusInToday
                            (clockinStatus config
                                           day
                                           (filter ((<=day).entryTime) entries)))
              in putStrLn (formatTime defaultTimeLocale "%F" day ++ " " ++ T.unpack (hoursIn day diff)))
  where days now =
          nub . map (min now . modL seconds (subtract 1) . modL day (+1) . startOfDay . entryTime)

-- | Print minutes.
printMinutes :: Config -> IO ()
printMinutes config =
  do entries <- readClockinEntries config
     now <- getLocalTime
     forM_ (days now entries)
           (\day ->
              let diff =
                    -1 * (statusInToday
                            (clockinStatus config
                                           day
                                           (filter ((<=day).entryTime) entries)))
              in putStrLn (formatTime defaultTimeLocale "%F" day ++ "\t" ++ show (minutesIn day diff)))
  where days now =
          nub . map (min now . modL seconds (subtract 1) . modL day (+1) . startOfDay . entryTime)

-- | Print hours.
printHours :: Config -> IO ()
printHours config =
  do entries <- readClockinEntries config
     now <- getLocalTime
     forM_ (days now entries)
           (\day ->
              let diff =
                    -1 * (statusInToday
                            (clockinStatus config
                                           day
                                           (filter ((<=day).entryTime) entries)))
              in putStrLn (formatTime defaultTimeLocale "%F" day ++ "\t" ++ show (hoursCountIn day diff)))
  where days now =
          nub . map (min now . modL seconds (subtract 1) . modL day (+1) . startOfDay . entryTime)

-- | Read in the log entries from file.
readClockinEntries :: Config -> IO [Entry]
readClockinEntries config =
  runResourceT (C.sourceFile (configFilePath config) $=
                C.lines $=
                CL.mapMaybe (decode . L.fromStrict) $$
                CL.consume)

-- | Make a human-readable representation of the status.
describeStatus :: LocalTime -> Status -> [Entry] -> Text
describeStatus now status entries =
  T.intercalate
    "\n"
    ["Current time is: " <> T.pack (formatTime defaultTimeLocale "%F %R" now)
    ,"Currently clocked " <> (if statusIn status
                                 then "IN "
                                 else "OUT ")
                          <> (if statusCurTimePeriod status == 0 && not (statusIn status)
                                 then ""
                                 else "(" <>
                                      diffTime (-1 * (statusCurTimePeriod status)) True <>
                                      ")")
    ,"Time spent today: " <> hoursIn now (-1 * (statusInToday status))
    ,"Remaining: " <> hoursRemaining now (statusRemainingToday status)
    ,"Log today:\n" <> T.intercalate "\n" (map describeEntry
                                               (filter ((>startOfDay now).entryTime) entries))
    ,if statusIn status then T.pack (formatTime defaultTimeLocale "%R (now)" now) else ""
    ]

-- | Describe an entry.
describeEntry :: Entry -> Text
describeEntry (In (ClockIn _ _ t)) = T.pack (formatTime defaultTimeLocale "%R" t) <> " in"
describeEntry (Out (ClockOut _ _ _ t)) = T.pack (formatTime defaultTimeLocale "%R" t) <> " out"

-- | Hours remaining.
hoursRemaining now (Due h) = hoursIn now h
hoursRemaining now (Credit h) = "-" <> hoursIn now h

-- | Show the number of hours in (or out, really).
hoursIn :: LocalTime -> NominalDiffTime -> Text
hoursIn now =
  T.pack .
  formatTime defaultTimeLocale "%R" .
  (`addLocalTime` startOfDay now)

-- | Show the number of minutes in (or out, really).
minutesIn :: LocalTime -> NominalDiffTime -> Int
minutesIn now =
  getL minutes .
  (`addLocalTime` startOfDay now)

-- | Show the number of hours in (or out, really).
hoursCountIn :: LocalTime -> NominalDiffTime -> Int
hoursCountIn now =
  getL hours .
  (`addLocalTime` startOfDay now)

-- | Make a short human-readable representation of the status, on one line.
onelinerStatus :: LocalTime -> Status -> Text
onelinerStatus now status =
  hoursIn now (-1 * (statusInToday status)) <>
  "/" <>
  hoursRemaining now (statusRemainingToday status) <>
  " (" <>
  (if statusIn status then "in" else "out") <>
  ")"

-- | Generate a status report of the current log.
clockinStatus :: Config -> LocalTime -> [Entry] -> Status
clockinStatus config now entries =
  Status clockedIn
         curPeriod
         todayDiff
         remaining
  where remaining
          | -1 * todayDiff < goalDiff =
            Due (diffLocalTime (addLocalTime goalDiff
                                             midnight)
                               (addLocalTime (-1 * todayDiff)
                                           midnight))
          | otherwise =
            Credit (diffLocalTime (addLocalTime (-1 * todayDiff)
                                              midnight)
                                  (addLocalTime goalDiff
                                                midnight))
        goalDiff = (60 * 60 * fromIntegral (configHoursPerDay config))
        todayDiff = inToday now descending
        clockedIn =
          fromMaybe False
                    (fmap (\i -> case i of
                                   In{} -> True
                                   _ -> False)
                          current)
        curPeriod = maybe 0 (diffLocalTime now . entryTime) current
        current = listToMaybe descending
        descending = reverse entries
        midnight = startOfDay now

-- | Get the time, if any, of an entry's clocking in.
entryTime :: Entry -> LocalTime
entryTime (In i) = timeLocalTime (inTime i)
entryTime (Out o) = timeLocalTime (outTime o)

-- | Get the project of the entry.
entryProject :: Entry -> Text
entryProject (In i) = inProject i
entryProject (Out o) = outProject o

-- | Get the starting time of the day of the given time.
startOfDay :: LocalTime -> LocalTime
startOfDay (LocalTime day _) = LocalTime day midnight

-- | How much time clocked in today? Expects a DESCENDING entry list.
--
-- If the clocking in time was yesterday, then don't include the work
-- from yesterday, only include the work for today starting from
-- midnight.
--
-- Stops traversing the list if it reaches an entry from yesterday.
inToday :: LocalTime -> [Entry] -> NominalDiffTime
inToday now = go now 0 . zip [0::Int ..]
  where go last total (((i,x):xs)) =
          case x of
            In{} | today -> go this (total + diffLocalTime this last) xs
                 | otherwise -> go this (total + diffLocalTime (startOfDay now) last) []
            Out{} | today -> go this total xs
                  | i == 0 -> go this total []
                  | otherwise -> go this total []
          where this = entryTime x
                today = localDay this == localDay now
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

-- Local time operations

getLocalTime :: IO LocalTime
getLocalTime = fmap zonedTimeToLocalTime getZonedTime

diffLocalTime :: LocalTime -> LocalTime -> NominalDiffTime
diffLocalTime t1 t2 = diffUTCTime (localTimeToUTC utc t1)
                                  (localTimeToUTC utc t2)

addLocalTime :: NominalDiffTime -> LocalTime -> LocalTime
addLocalTime d = tmap (addUTCTime d)

tmap :: (UTCTime -> UTCTime) -> LocalTime -> LocalTime
tmap f = utcToLocalTime utc . f . localTimeToUTC utc

-- | A local time. The only reason for this is to avoid the orphan instance.
newtype Time = Time
  { timeLocalTime :: LocalTime }
  deriving (Show,ParseTime,FormatTime,Generic)

instance ToJSON Time where
  toJSON =
    toJSON . formatTime defaultTimeLocale "%F %T"

instance FromJSON Time where
  parseJSON s =
    do s <- parseJSON s
       case parseTime defaultTimeLocale "%F %T" s of
         Nothing -> fail "Couldn't parse local time."
         Just t -> return t
