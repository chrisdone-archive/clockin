-- | Main entry point to clockin.
--
-- Track clocking in and out of work.

module Main where

import Clockin
import Data.Maybe
import Data.Text (pack)
import System.Environment

-- | Main entry point.
main :: IO ()
main = do
  (cmd:(~(project:task))) <- getArgs
  config <- getClockinConfig
  case cmd of
    "in" -> clockIn config
                    (pack project)
                    (fmap pack (listToMaybe task))
    "out" -> clockOut config
                      (pack project)
                      (fmap pack (listToMaybe task))
                      Nothing
    "status" -> printClockinStatus config
    "days" -> printSparkDays config
    "minutes" -> printMinutes config
    "hours" -> printHours config
    "toggle" -> do entries <- readClockinEntries config
                   case reverse entries of
                     (entry:_) ->
                       do now <- getLocalTime
                          if statusIn (clockinStatus config now entries)
                             then clockOut config (entryProject entry) Nothing Nothing
                             else clockIn config (entryProject entry) Nothing
                     _ -> error "No previous project to clock in/out of"
    _ -> error "clockin <in/out> <project> [task] or clockin <status/spark-days>"
