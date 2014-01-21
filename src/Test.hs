{-# LANGUAGE OverloadedStrings #-}

-- | Tests.

module Main where

import Clockin
import Data.Maybe
import Data.Monoid
import Data.Time.Format
import Data.Time.LocalTime
import System.Exit
import System.Locale

-- | Test that the 'inToday' function works properly.
main :: IO ()
main =
  either (\(ts,expected,actual,cur) ->
            do mapM_ (putStrLn . asEntry) ts
               putStrLn ("Current time: " <> cur)
               putStrLn ("Expected: " <> show expected)
               putStrLn ("Actual: " <> show actual)
               exitFailure)
        (const (putStrLn "OK"))
        (sequence [matches [i "10:00",o "11:00"] "04:00" (-1 * (1*60*60))
                  ,matches [i "00:00",o "02:00",i "03:00",o "04:00"] "05:00" (-1 * (3*60*60))
                  ,matches [i "00:00",o "02:00",i "03:00"] "05:00" (-1 * (4 * 60 * 60))
                  ,matches [i' "10:00",o' "11:00"] "01:00" 0])
  where asEntry (In i) = "i " <> formatTime defaultTimeLocale "%F %R" (inTime i)
        asEntry (Out o) = "o " <> formatTime defaultTimeLocale "%F %R" (outTime o)
        matches times n e = if r == e then Right () else Left (times,e,r,c)
          where c = formatTime defaultTimeLocale "%F %R" (now' n)
                r = inToday (now' n) (reverse times)
        now' :: String -> LocalTime
        now' n = (fromJust . parseTime defaultTimeLocale "%F%R" . (today<>)) n
        i n =
          In (ClockIn "Some project"
                      Nothing
                      ((fromJust . parseTime defaultTimeLocale "%F%R" . (today<>)) n))
        i' n =
          In (ClockIn "Some project"
                      Nothing
                      ((fromJust . parseTime defaultTimeLocale "%F%R" . (yesterday<>)) n))
        o n =
          Out (ClockOut "Some project"
                        Nothing
                        Nothing
                        ((fromJust . parseTime defaultTimeLocale "%F%R" . (today<>)) n))
        o' n =
           Out (ClockOut "Some project"
                         Nothing
                         Nothing
                         ((fromJust . parseTime defaultTimeLocale "%F%R" . (yesterday<>)) n))
        today = "2014-01-20"
        yesterday = "2014-01-19"
