-- | Main entry point to clockin.
--
-- Track clocking in and out of work.

module Main where

import Clockin
import Data.Maybe
import Data.Text
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
    _ -> error "clockin <in/out> <project> [task] or clockin <status/spark-days>"
