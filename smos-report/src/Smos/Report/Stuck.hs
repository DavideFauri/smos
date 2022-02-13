{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Stuck where

import Control.DeepSeq
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Tree
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Safe
import Smos.Data

data StuckReport = StuckReport
  { stuckReportEntries :: [StuckReportEntry]
  }
  deriving (Show, Eq, Generic)

instance Validity StuckReport

instance NFData StuckReport

data StuckReportEntry = StuckReportEntry
  { stuckReportEntryFilePath :: Path Rel File,
    stuckReportEntryState :: Maybe TodoState,
    stuckReportEntryHeader :: Header,
    stuckReportEntryLatestChange :: Maybe UTCSecond
  }
  deriving (Show, Eq, Generic)

instance Validity StuckReportEntry

instance NFData StuckReportEntry

makeStuckReport :: [StuckReportEntry] -> StuckReport
makeStuckReport = StuckReport . sortStuckEntries

sortStuckEntries :: [StuckReportEntry] -> [StuckReportEntry]
sortStuckEntries = sortOn stuckReportEntryLatestChange

makeStuckReportEntry :: TimeZone -> Path Rel File -> SmosFile -> Maybe StuckReportEntry
makeStuckReportEntry tz stuckReportEntryFilePath sf = do
  e <- latestEntryInSmosFile tz sf
  let stuckReportEntryHeader = entryHeader e
      stuckReportEntryState = entryState e
      stuckReportEntryLatestChange = latestTimestampInEntry tz e
  pure StuckReportEntry {..}

latestEntryInSmosFile :: TimeZone -> SmosFile -> Maybe Entry
latestEntryInSmosFile tz =
  (>>= lastMay)
    . headMay
    . groupBy ((==) `on` latestTimestampInEntry tz)
    . sortOn (Down . latestTimestampInEntry tz)
    . concatMap flatten
    . smosFileForest

latestTimestampInEntry :: TimeZone -> Entry -> Maybe UTCSecond
latestTimestampInEntry tz e@Entry {..} =
  maximumMay $
    catMaybes $
      concat
        [ [ latestStateChange entryStateHistory,
            latestClockChange entryLogbook
          ],
          [ latestTimestamp tz entryTimestamps | not (entryIsDone e)
          ]
        ]

latestStateChange :: StateHistory -> Maybe UTCSecond
latestStateChange (StateHistory shes) =
  case shes of
    [] -> Nothing
    (she : _) -> Just $ stateHistoryEntryTimestamp she

latestClockChange :: Logbook -> Maybe UTCSecond
latestClockChange = \case
  LogOpen t _ -> Just t
  LogClosed les -> case les of
    [] -> Nothing
    (le : _) -> Just $ logbookEntryEnd le

latestTimestamp :: TimeZone -> Map TimestampName Timestamp -> Maybe UTCSecond
latestTimestamp tz =
  fmap snd
    . M.lookupMax
    . M.map
      ( localSecondToUTCSecond tz
          . timestampLocalSecond
      )
