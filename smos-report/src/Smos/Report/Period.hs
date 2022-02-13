{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Period where

import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.TimeBlock

data Period
  = Yesterday
  | Today
  | Tomorrow
  | LastWeek
  | ThisWeek
  | NextWeek
  | LastMonth -- TODO add year
  | ThisMonth
  | NextMonth
  | LastYear
  | ThisYear
  | NextYear
  | AllTime
  | BeginOnly LocalSecond
  | EndOnly LocalSecond
  | BeginEnd LocalSecond LocalSecond -- If end is before begin, this matches nothing
  deriving (Show, Eq, Generic)

instance Validity Period

yearPeriod :: YearNumber -> Period
yearPeriod y = BeginEnd monthStart monthEnd
  where
    monthStart :: LocalSecond
    monthStart =
      LocalSecond
        (fromGregorian y 1 1)
        (SecondOfDay 0)
    monthEnd :: LocalSecond
    monthEnd =
      LocalSecond
        (fromGregorian (y + 1) 1 1)
        (SecondOfDay 0)

monthPeriod :: MonthNumber -> Period
monthPeriod MonthNumber {..} = BeginEnd monthStart monthEnd
  where
    monthStart :: LocalSecond
    monthStart =
      LocalSecond
        (fromGregorian monthNumberYear monthNumberMonth 1)
        (SecondOfDay 0)
    monthEnd :: LocalSecond
    monthEnd =
      LocalSecond
        (fromGregorian monthNumberYear (monthNumberMonth + 1) 1)
        (SecondOfDay 0) -- FIXME this can wrong at the end of the year

weekPeriod :: WeekNumber -> Period
weekPeriod WeekNumber {..} = BeginEnd weekStart weekEnd
  where
    weekStart :: LocalSecond
    weekStart =
      LocalSecond
        (fromWeekDate weekNumberYear weekNumberWeek 1)
        (SecondOfDay 0)
    weekEnd :: LocalSecond
    weekEnd =
      LocalSecond
        (fromWeekDate weekNumberYear (weekNumberWeek + 1) 1)
        (SecondOfDay 0) -- FIXME this can wrong at the end of the year

dayPeriod :: Day -> Period
dayPeriod d = BeginEnd dayStart dayEnd
  where
    dayStart = LocalSecond {localSecondDay = d, localSecondOfDay = SecondOfDay 0}
    dayEnd = LocalSecond {localSecondDay = addDays 1 d, localSecondOfDay = SecondOfDay 0}

filterPeriodLocal :: ZonedTime -> Period -> LocalSecond -> Bool
filterPeriodLocal now p l =
  ( case p of
      AllTime -> const True
      Yesterday -> filterBetween yesterdayStart yesterdayEnd
      Today -> filterBetween todayStart todayEnd
      Tomorrow -> filterBetween tomorrowStart tomorrowEnd
      LastWeek -> filterBetween lastWeekStart lastWeekEnd
      ThisWeek -> filterBetween thisWeekStart thisWeekEnd
      NextWeek -> filterBetween nextWeekStart nextWeekEnd
      LastMonth -> filterBetween lastMonthStart lastMonthEnd
      ThisMonth -> filterBetween thisMonthStart thisMonthEnd
      NextMonth -> filterBetween nextMonthStart nextMonthEnd
      LastYear -> filterBetween lastYearStart lastYearEnd
      ThisYear -> filterBetween thisYearStart thisYearEnd
      NextYear -> filterBetween nextYearStart nextYearEnd
      BeginOnly begin -> (begin <=)
      EndOnly end -> (< end)
      BeginEnd begin end -> filterBetween begin end
  )
    l
  where
    today :: Day
    today = localDay $ zonedTimeToLocalTime now
    filterBetween :: LocalSecond -> LocalSecond -> LocalSecond -> Bool
    filterBetween start end lt = start <= lt && lt < end
    yesterdayStart :: LocalSecond
    yesterdayStart =
      LocalSecond
        { localSecondDay = addDays (-1) today,
          localSecondOfDay = SecondOfDay 0
        }
    yesterdayEnd :: LocalSecond
    yesterdayEnd = todayStart
    todayStart :: LocalSecond
    todayStart =
      LocalSecond
        { localSecondDay = today,
          localSecondOfDay = SecondOfDay 0
        }
    todayEnd :: LocalSecond
    todayEnd =
      LocalSecond
        { localSecondDay = addDays 1 today,
          localSecondOfDay = SecondOfDay 0
        }
    tomorrowStart :: LocalSecond
    tomorrowStart = todayEnd
    tomorrowEnd :: LocalSecond
    tomorrowEnd =
      LocalSecond
        { localSecondDay = addDays 2 today,
          localSecondOfDay = SecondOfDay 0
        }
    lastWeekStart :: LocalSecond
    lastWeekStart =
      let (y, wn, _) = toWeekDate today
       in LocalSecond (fromWeekDate y (wn - 1) 1) (SecondOfDay 0) -- TODO this will fail around newyear
    lastWeekEnd :: LocalSecond
    lastWeekEnd = thisWeekStart
    thisWeekStart :: LocalSecond
    thisWeekStart =
      let (y, wn, _) = toWeekDate today
       in LocalSecond (fromWeekDate y wn 1) (SecondOfDay 0)
    thisWeekEnd :: LocalSecond
    thisWeekEnd =
      let (y, wn, _) = toWeekDate today
       in LocalSecond (fromWeekDate y (wn + 1) 1) (SecondOfDay 0) -- FIXME this can wrong at the end of the year
    nextWeekStart :: LocalSecond
    nextWeekStart = thisWeekEnd
    nextWeekEnd :: LocalSecond
    nextWeekEnd =
      let (y, wn, _) = toWeekDate today
       in LocalSecond (fromWeekDate y (wn + 2) 1) (SecondOfDay 0) -- FIXME this can wrong at the end of the year
    lastMonthStart :: LocalSecond
    lastMonthStart =
      let (y, m, _) = toGregorian today
       in LocalSecond (fromGregorian y (m - 1) 1) (SecondOfDay 0) -- FIXME This will fail around newyear
    lastMonthEnd :: LocalSecond
    lastMonthEnd = thisMonthStart
    thisMonthStart :: LocalSecond
    thisMonthStart =
      let (y, m, _) = toGregorian today
       in LocalSecond (fromGregorian y m 1) (SecondOfDay 0)
    thisMonthEnd :: LocalSecond
    thisMonthEnd =
      let (y, m, _) = toGregorian today
       in LocalSecond (fromGregorian y m 31) (SecondOfDay 0)
    nextMonthStart :: LocalSecond
    nextMonthStart = thisMonthEnd
    nextMonthEnd :: LocalSecond
    nextMonthEnd =
      let (y, m, _) = toGregorian today
       in LocalSecond (fromGregorian y (m + 1) 31) (SecondOfDay 0) -- FIXME This will fail around newyear
    lastYearStart :: LocalSecond
    lastYearStart =
      let (y, _, _) = toGregorian today
       in LocalSecond (fromGregorian (y - 1) 1 1) (SecondOfDay 0) -- FIXME This will fail around newyear
    lastYearEnd :: LocalSecond
    lastYearEnd = thisYearEnd
    thisYearStart :: LocalSecond
    thisYearStart =
      let (y, _, _) = toGregorian today
       in LocalSecond (fromGregorian y 1 1) (SecondOfDay 0)
    thisYearEnd :: LocalSecond
    thisYearEnd =
      let (y, _, _) = toGregorian today
       in LocalSecond (fromGregorian y 12 31) (SecondOfDay 0)
    nextYearStart :: LocalSecond
    nextYearStart = thisYearEnd
    nextYearEnd :: LocalSecond
    nextYearEnd =
      let (y, _, _) = toGregorian today
       in LocalSecond (fromGregorian (y + 1) 12 31) (SecondOfDay 0) -- FIXME this will fail around newyear

filterPeriod :: ZonedTime -> Period -> UTCSecond -> Bool
filterPeriod now p u =
  let tz :: TimeZone
      tz = zonedTimeZone now
   in filterPeriodLocal now p $ localTimeToLocalSecond $ utcToLocalTime tz $ utcSecondToUTCTime u
