{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Report.Clock
  ( module Smos.Report.Clock,
    module Smos.Report.Clock.Types,
  )
where

import Cursor.Simple.Forest
import Cursor.Simple.Tree
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Validity
import Data.Validity.Path ()
import Lens.Micro
import Path
import Smos.Data
import Smos.Report.Clock.Types
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.Streaming
import Smos.Report.TimeBlock

-- | Reset the timers of every entry that doesn't match the filter to zero
zeroOutByFilter :: EntryFilter -> Path Rel File -> SmosFile -> SmosFile
zeroOutByFilter f rp sf =
  let cursors = forestCursors $ smosFileForest sf
   in sf {smosFileForest = map (fmap go) cursors}
  where
    go :: ForestCursor Entry -> Entry
    go fc =
      ( if filterPredicate f (rp, fc)
          then id
          else zeroOutEntry
      )
        (fc ^. (forestCursorSelectedTreeL . treeCursorCurrentL))

zeroOutEntry :: Entry -> Entry
zeroOutEntry e = e {entryLogbook = emptyLogbook}

findFileTimes :: UTCTime -> Path Rel File -> SmosFile -> Maybe FileTimes
findFileTimes now rp sf = do
  ne <- goF (smosFileForest sf)
  pure $ FileTimes {clockTimeFile = rp, clockTimeForest = ne}
  where
    goF :: Forest Entry -> Maybe (TForest HeaderTimes)
    goF = NE.nonEmpty . mapMaybe goT
    goT :: Tree Entry -> Maybe (TTree HeaderTimes)
    goT (Node e ts_) =
      case goF ts_ of
        Nothing -> do
          hts <- headerTimesNonEmpty $ findHeaderTimes now e
          pure $ TLeaf hts
        Just f -> pure $ TBranch (findHeaderTimes now e) f

findHeaderTimes :: UTCTime -> Entry -> HeaderTimes []
findHeaderTimes now Entry {..} =
  case entryLogbook of
    LogOpen s es -> ht $ (LogbookEntry {logbookEntryStart = s, logbookEntryEnd = utcTimeToUTCSecond now}) : es
    LogClosed es -> ht es
  where
    ht es = HeaderTimes {headerTimesHeader = entryHeader, headerTimesEntries = es}

headerTimesList :: HeaderTimes NonEmpty -> HeaderTimes []
headerTimesList hts =
  HeaderTimes
    { headerTimesHeader = headerTimesHeader hts,
      headerTimesEntries = NE.toList $ headerTimesEntries hts
    }

headerTimesNonEmpty :: HeaderTimes [] -> Maybe (HeaderTimes NonEmpty)
headerTimesNonEmpty hts = do
  ne <- NE.nonEmpty $ headerTimesEntries hts
  pure $ HeaderTimes {headerTimesHeader = headerTimesHeader hts, headerTimesEntries = ne}

trimHeaderTimes :: ZonedTime -> Period -> HeaderTimes [] -> HeaderTimes []
trimHeaderTimes zt cp ht =
  let es' = mapMaybe (trimLogbookEntry zt cp) $ headerTimesEntries ht
   in ht {headerTimesEntries = es'}

trimLogbookEntry :: ZonedTime -> Period -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntry now cp =
  case cp of
    AllTime -> pure
    Yesterday -> trimLogbookEntryToDay tz (pred today)
    Today -> trimLogbookEntryToDay tz today
    Tomorrow -> trimLogbookEntryToDay tz (succ today)
    LastWeek -> trimLogbookEntryTo tz lastWeekStart lastWeekEnd
    ThisWeek -> trimLogbookEntryTo tz thisWeekStart thisWeekEnd
    NextWeek -> trimLogbookEntryTo tz nextWeekStart nextWeekEnd
    LastMonth -> trimLogbookEntryTo tz lastMonthStart lastMonthEnd
    ThisMonth -> trimLogbookEntryTo tz thisMonthStart thisMonthEnd
    NextMonth -> trimLogbookEntryTo tz nextMonthStart nextMonthEnd
    LastYear -> trimLogbookEntryTo tz lastYearStart lastYearEnd
    ThisYear -> trimLogbookEntryTo tz thisYearStart thisYearEnd
    NextYear -> trimLogbookEntryTo tz nextYearStart nextYearEnd
    BeginOnly begin -> trimLogbookEntryToM tz (Just begin) Nothing
    EndOnly end -> trimLogbookEntryToM tz Nothing (Just end)
    BeginEnd begin end -> trimLogbookEntryTo tz begin end
  where
    tz :: TimeZone
    tz = zonedTimeZone now
    today :: Day
    today = localDay (zonedTimeToLocalTime now)
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

trimLogbookEntryToDay :: TimeZone -> Day -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntryToDay tz d = trimLogbookEntryTo tz dayStart dayEnd
  where
    dayStart = LocalSecond d (SecondOfDay 0)
    dayEnd = LocalSecond (addDays 1 d) (SecondOfDay 0)

trimLogbookEntryTo :: TimeZone -> LocalSecond -> LocalSecond -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntryTo tz begin end = trimLogbookEntryToM tz (Just begin) (Just end)

trimLogbookEntryToM ::
  TimeZone -> Maybe LocalSecond -> Maybe LocalSecond -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntryToM tz mBegin mEnd LogbookEntry {..} =
  constructValid $
    LogbookEntry
      { logbookEntryStart = case mBegin of
          Nothing -> logbookEntryStart
          Just begin ->
            if toLocal (utcSecondToUTCTime logbookEntryStart) >= begin
              then logbookEntryStart
              else fromLocal begin,
        logbookEntryEnd = case mEnd of
          Nothing -> logbookEntryEnd
          Just end ->
            if toLocal (utcSecondToUTCTime logbookEntryEnd) < end
              then logbookEntryEnd
              else fromLocal end
      }
  where
    toLocal :: UTCTime -> LocalSecond
    toLocal = localTimeToLocalSecond . utcToLocalTime tz
    fromLocal :: LocalSecond -> UTCSecond
    fromLocal = localSecondToUTCSecond tz

divideIntoClockTimeBlocks :: ZonedTime -> TimeBlock -> [FileTimes] -> [ClockTimeBlock Text]
divideIntoClockTimeBlocks zt cb cts =
  case cb of
    OneBlock -> [Block {blockTitle = "All Time", blockEntries = cts}]
    YearBlock -> divideClockTimeIntoTimeBlocks formatYearTitle dayYear yearPeriod
    MonthBlock -> divideClockTimeIntoTimeBlocks formatMonthTitle dayMonth monthPeriod
    WeekBlock -> divideClockTimeIntoTimeBlocks formatWeekTitle dayWeek weekPeriod
    DayBlock -> divideClockTimeIntoTimeBlocks formatDayTitle id dayPeriod
  where
    divideClockTimeIntoTimeBlocks ::
      (Ord t, Enum t) => (t -> Text) -> (Day -> t) -> (t -> Period) -> [ClockTimeBlock Text]
    divideClockTimeIntoTimeBlocks format fromDay toPeriod =
      map (mapBlockTitle format) $
        combineBlocksByName $
          concatMap
            ( divideClockTimeIntoBlocks
                zt
                ( fromDay
                    . localDay
                    . utcToLocalTime (zonedTimeZone zt)
                    . utcSecondToUTCTime
                )
                toPeriod
            )
            cts

divideClockTimeIntoBlocks ::
  forall t.
  (Enum t, Ord t) =>
  ZonedTime ->
  (UTCSecond -> t) ->
  (t -> Period) ->
  FileTimes ->
  [ClockTimeBlock t]
divideClockTimeIntoBlocks zt func toPeriod =
  map (uncurry makeClockTimeBlock) . sortAndGroupCombineOrd . divideFileTimes
  where
    makeClockTimeBlock :: a -> [FileTimes] -> ClockTimeBlock a
    makeClockTimeBlock n cts = Block {blockTitle = n, blockEntries = cts}
    divideFileTimes :: FileTimes -> [(t, FileTimes)]
    divideFileTimes fts =
      mapMaybe (\d -> (,) d <$> trimFileTimes zt (toPeriod d) fts) (S.toList $ fileTimesDays fts)
    fileTimesDays :: FileTimes -> Set t
    fileTimesDays = goTF . clockTimeForest
      where
        goTF :: TForest HeaderTimes -> Set t
        goTF = S.unions . map goTT . NE.toList
        goTT :: TTree HeaderTimes -> Set t
        goTT (TLeaf hts) = goHT $ headerTimesList hts
        goTT (TBranch hts tf) = goHT hts `S.union` goTF tf
        goHT :: HeaderTimes [] -> Set t
        goHT = S.unions . map logbookEntryDays . headerTimesEntries
        logbookEntryDays :: LogbookEntry -> Set t
        logbookEntryDays LogbookEntry {..} =
          S.fromList [func logbookEntryStart .. func logbookEntryEnd]

trimFileTimesToDay :: TimeZone -> Day -> FileTimes -> Maybe FileTimes
trimFileTimesToDay tz d fts = (\f -> fts {clockTimeForest = f}) <$> goTF (clockTimeForest fts)
  where
    goTF :: TForest HeaderTimes -> Maybe (TForest HeaderTimes)
    goTF ts = do
      let ts' = mapMaybe goTT $ NE.toList ts
      NE.nonEmpty ts'
    goTT :: TTree HeaderTimes -> Maybe (TTree HeaderTimes)
    goTT (TLeaf hts) = do
      hts' <- headerTimesNonEmpty $ goHT $ headerTimesList hts
      pure $ TLeaf hts'
    goTT (TBranch hts tf) =
      case goTF tf of
        Nothing -> TLeaf <$> headerTimesNonEmpty (goHT hts)
        Just f -> pure $ TBranch (goHT hts) f
    goHT :: HeaderTimes [] -> HeaderTimes []
    goHT hts =
      hts {headerTimesEntries = mapMaybe (trimLogbookEntryToDay tz d) (headerTimesEntries hts)}

sortAndGroupCombineOrd :: Ord a => [(a, b)] -> [(a, [b])]
sortAndGroupCombineOrd = sortGroupCombine compare

sortGroupCombine :: (a -> a -> Ordering) -> [(a, b)] -> [(a, [b])]
sortGroupCombine func =
  map combine . groupBy ((\a1 a2 -> func a1 a2 == EQ) `on` fst) . sortBy (func `on` fst)
  where
    combine [] = error "cannot happen due to groupBy above"
    combine ts@((a, _) : _) = (a, map snd ts)

makeClockTable :: [ClockTimeBlock Text] -> ClockTable
makeClockTable = map makeClockTableBlock

makeClockTableBlock :: ClockTimeBlock Text -> ClockTableBlock
makeClockTableBlock Block {..} =
  Block {blockTitle = blockTitle, blockEntries = map makeClockTableFile blockEntries}

makeClockTableFile :: FileTimes -> ClockTableFile
makeClockTableFile FileTimes {..} =
  ClockTableFile {clockTableFile = clockTimeFile, clockTableForest = unTForest clockTimeForest}

unTForest :: TForest HeaderTimes -> Forest ClockTableHeaderEntry
unTForest = map unTTree . NE.toList

unTTree :: TTree HeaderTimes -> Tree ClockTableHeaderEntry
unTTree (TLeaf hts) = Node (makeClockTableHeaderEntry $ headerTimesList hts) []
unTTree (TBranch hts tf) = Node (makeClockTableHeaderEntry hts) (unTForest tf)

makeClockTableHeaderEntry :: HeaderTimes [] -> ClockTableHeaderEntry
makeClockTableHeaderEntry HeaderTimes {..} =
  ClockTableHeaderEntry
    { clockTableHeaderEntryHeader = headerTimesHeader,
      clockTableHeaderEntryTime = sumLogbookEntryTime headerTimesEntries
    }

sumLogbookEntryTime :: [LogbookEntry] -> NominalDiffTime
sumLogbookEntryTime = foldl' (+) 0 . map logbookEntryDiffTime

trimFileTimes :: ZonedTime -> Period -> FileTimes -> Maybe FileTimes
trimFileTimes zt cp fts = do
  f <- goF $ clockTimeForest fts
  pure $ fts {clockTimeForest = f}
  where
    goF :: TForest HeaderTimes -> Maybe (TForest HeaderTimes)
    goF tf = NE.nonEmpty $ mapMaybe goT $ NE.toList tf
    goT :: TTree HeaderTimes -> Maybe (TTree HeaderTimes)
    goT (TLeaf hts) = TLeaf <$> headerTimesNonEmpty (trimHeaderTimes zt cp (headerTimesList hts))
    goT (TBranch hts tf) =
      case goF tf of
        Nothing -> TLeaf <$> headerTimesNonEmpty (trimHeaderTimes zt cp hts)
        Just f -> pure $ TBranch (trimHeaderTimes zt cp hts) f
