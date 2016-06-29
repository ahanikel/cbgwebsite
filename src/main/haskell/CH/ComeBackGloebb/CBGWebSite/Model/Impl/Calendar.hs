{-# LANGUAGE TypeSynonymInstances #-}

module CH.ComeBackGloebb.CBGWebSite.Model.Impl.Calendar () where

import           Data.DateTime (DateTime, addSeconds, formatDateTime,
                                fromGregorian', fromSeconds, toSeconds)
import           Data.List     (groupBy)
import           Debug.Trace

toDayOfWeek :: DateTime -> Int
toDayOfWeek d = read $ formatDateTime "%u" d

daysToSeconds :: Int -> Integer
daysToSeconds d = fromIntegral d * 60 * 60 * 24

addDays :: DateTime -> Int -> DateTime
date `addDays` days = daysToSeconds days `addSeconds` date

subtractDays :: DateTime -> Int -> DateTime
date `subtractDays` days = (daysToSeconds days * (-1)) `addSeconds` date

mondayBefore :: DateTime -> DateTime
mondayBefore date = date `subtractDays` days
  where
    days = toDayOfWeek date - 1

sundayAfter :: DateTime -> DateTime
sundayAfter date = date `addDays` days
  where
    days = 7 - toDayOfWeek date

instance Enum DateTime where
  toEnum = fromSeconds . toInteger . (* 86400)
  fromEnum = fromIntegral . (`div` 86400) . toSeconds

calendarForMonth :: Int -> Int -> [[DateTime]]
calendarForMonth year month = map (map fst) $ groupBy mondays $ zip [startOfCal .. endOfCal] (cycle [1 .. 7])
  where startOfCal = mondayBefore $ fromGregorian' (fromIntegral year) month 1
        endOfCal   = sundayAfter  $ fromGregorian' (fromIntegral year) (month + 1) 1 `subtractDays` 1
        mondays (_, x) (_, y) = y > 1
