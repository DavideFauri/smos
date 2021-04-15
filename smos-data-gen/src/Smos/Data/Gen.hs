{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import Data.List
import Data.SemVer as Version
import qualified Data.Text as T
import Data.Time
import Data.Word
import Smos.Data
import Test.QuickCheck

instance GenValid a => GenValid (Versioned a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Version where
  shrinkValid _ = [] -- No shrinking for now
  genValid =
    let genComponent = fromIntegral <$> (genValid :: Gen Word32) -- This doesn't generate version numbers >2^32 but that's fine.
     in version <$> genComponent <*> genComponent <*> genComponent <*> pure [] <*> pure [] -- No identifiers for now

instance GenValid SmosFile where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ForYaml a)

instance GenValid a => GenValid (ForYaml a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Entry where
  genValid =
    sized $ \size -> do
      (a, b, c, d, e, f, g) <- genSplit7 size
      entryHeader <- resize a genValid
      entryContents <- resize b genValid
      entryTimestamps <- resize c genValid
      entryProperties <- resize d genValid
      entryStateHistory <- resize e genValid
      entryTags <- resize f genValid
      entryLogbook <- resize g genValid
      pure Entry {..}
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Header where
  genValid = Header <$> genTextBy genHeaderChar
  shrinkValid = shrinkValidStructurally

genHeaderChar :: Gen Char
genHeaderChar = choose (minBound, maxBound) `suchThat` validHeaderChar

instance GenValid Contents where
  genValid = Contents <$> genTextBy genContentsChar
  shrinkValid = shrinkValidStructurally

genContentsChar :: Gen Char
genContentsChar = choose (minBound, maxBound) `suchThat` validContentsChar

instance GenValid PropertyName where
  genValid = PropertyName <$> genTextBy genPropertyNameChar
  shrinkValid = shrinkValidStructurally

genPropertyNameChar :: Gen Char
genPropertyNameChar = choose (minBound, maxBound) `suchThat` validPropertyNameChar

instance GenValid PropertyValue where
  genValid = PropertyValue <$> genTextBy genPropertyValueChar
  shrinkValid = shrinkValidStructurally

genPropertyValueChar :: Gen Char
genPropertyValueChar = choose (minBound, maxBound) `suchThat` validPropertyValueChar

instance GenValid TimestampName where
  genValid = TimestampName <$> genTextBy genTimestampNameChar
  shrinkValid = shrinkValidStructurally

genTimestampNameChar :: Gen Char
genTimestampNameChar = choose (minBound, maxBound) `suchThat` validTimestampNameChar

instance GenUnchecked Timestamp

instance GenValid Timestamp where
  genValid =
    (`suchThat` isValid) $
      oneof
        [ TimestampDay <$> genValid,
          TimestampLocalTime <$> genValid,
          TimestampZonedTime
            <$> ( ZonedTime <$> genValid
                    <*> ( ( \tz ->
                              tz
                                { -- Timezones with large offsets are not
                                  -- round-trip-able so we limit it to 60*24-1.
                                  -- These large offsets don't occur in
                                  -- practice (because they cannot be parsed)
                                  -- so that's fine.
                                  timeZoneMinutes = min 1439 $ max (-1439) $ timeZoneMinutes tz,
                                  -- Timezones with UTF16 suggorate codepoint
                                  -- characters don't roundtrip either, in fact
                                  -- they produce runtime exceptions. These
                                  -- weird offsets don't occur in practice so
                                  -- that's fine.
                                  timeZoneName = T.unpack $ T.pack $ timeZoneName tz
                                }
                          )
                            <$> genValid
                        )
                )
        ]

  -- A custom shrinker because we can shrink accross constructors
  shrinkValid = \case
    TimestampDay d -> TimestampDay <$> shrinkValid d
    TimestampLocalTime lt ->
      TimestampDay (localDay lt) :
      (TimestampLocalTime <$> shrinkValid lt)
    TimestampZonedTime zt ->
      TimestampLocalTime (zonedTimeToLocalTime zt) :
      (TimestampZonedTime <$> shrinkValid zt)

instance GenValid TodoState where
  genValid = TodoState <$> genTextBy genTodoStateChar
  shrinkValid = shrinkValidStructurally

genTodoStateChar :: Gen Char
genTodoStateChar = choose (minBound, maxBound) `suchThat` validTodoStateChar

instance GenValid StateHistory where
  genValid = StateHistory . sort <$> genValid
  shrinkValid =
    fmap StateHistory
      . shrinkList (\(StateHistoryEntry mts ts) -> StateHistoryEntry <$> shrinkValid mts <*> pure ts)
      . unStateHistory

instance GenValid StateHistoryEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Tag where
  genValid = Tag <$> genTextBy genTagChar
  shrinkValid = shrinkValidStructurally

genTagChar :: Gen Char
genTagChar = choose (minBound, maxBound) `suchThat` validTagChar

instance GenUnchecked Logbook

instance GenValid Logbook where
  genValid =
    let genPositiveNominalDiffTime = realToFrac . abs <$> (genValid :: Gen Rational)
        listOfLogbookEntries =
          sized $ \n -> do
            ss <- arbPartition n
            let go [] = pure []
                go (s : rest) = do
                  lbes <- go rest
                  cur <-
                    resize s $
                      case lbes of
                        [] -> genValid
                        (p : _) ->
                          sized $ \m -> do
                            (a, b) <- genSplit m
                            ndt1 <- resize a genPositiveNominalDiffTime
                            ndt2 <- resize b genPositiveNominalDiffTime
                            let start = addUTCTime ndt1 (logbookEntryEnd p)
                                end = addUTCTime ndt2 start
                            pure $ LogbookEntry {logbookEntryStart = start, logbookEntryEnd = end}
                  pure $ cur : lbes
            go ss
     in oneof
          [ LogClosed <$> listOfLogbookEntries,
            do
              lbes <- listOfLogbookEntries
              l <-
                case lbes of
                  [] -> genValid
                  (lbe : _) -> do
                    ndt <- genPositiveNominalDiffTime
                    pure $ addUTCTime ndt $ logbookEntryEnd lbe
              pure $ LogOpen l lbes
          ]

instance GenUnchecked LogbookEntry where
  shrinkUnchecked _ = [] -- There's no point.

instance GenValid LogbookEntry where
  genValid =
    sized $ \n -> do
      (a, b) <- genSplit n
      start <- resize a genValid
      ndt <- resize b $ realToFrac . abs <$> (genValid :: Gen Rational)
      let end = addUTCTime ndt start
      pure LogbookEntry {logbookEntryStart = start, logbookEntryEnd = end}
