{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Data.TypesSpec
  ( spec,
  )
where

import Control.Monad
import Data.Aeson as JSON
import Data.Data
import Data.Time
import Smos.Data.Gen ()
import Smos.Data.Types
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils

spec :: Spec
spec = do
  genValidSpec @Header
  jsonSpec @Header
  textLikeJSONValid @Header
  genValidSpec @Contents
  jsonSpec @Contents
  textLikeJSONValid @Contents
  genValidSpec @PropertyName
  jsonSpec @PropertyName
  textLikeJSONValid @PropertyName
  -- If you remove this, the tests for json for sorters, filters and projections will fail in smos-report-gen
  describe "validPropertyNameChar" $ do
    it "says that ')' is invalid" $
      validPropertyNameChar ')' `shouldBe` False
    it
      "says that '(' is invalid"
      $ validPropertyNameChar '(' `shouldBe` False
  genValidSpec @PropertyValue
  jsonSpec @PropertyValue
  textLikeJSONValid @PropertyValue
  genValidSpec @TimestampName
  jsonSpec @TimestampName
  textLikeJSONValid @TimestampName
  genValidSpec @Timestamp
  jsonSpec @Timestamp
  textLikeJSONValid @Timestamp
  describe "parseTimestampString" $
    it "parses whatever timestampString outputs" $
      forAllValid $
        \ts -> parseTimestampString (timestampString ts) `shouldBe` Right ts
  describe "parseTimestampText" $
    it "parses whatever timestampText outputs" $
      forAllValid $
        \ts -> parseTimestampText (timestampText ts) `shouldBe` Right ts
  genValidSpec @TodoState
  jsonSpec @TodoState
  textLikeJSONValid @TodoState
  ordSpec @StateHistory
  genValidSpec @StateHistory
  jsonSpec @StateHistory
  ordSpec @StateHistoryEntry
  genValidSpec @StateHistoryEntry
  jsonSpec @StateHistoryEntry
  genValidSpec @Tag
  jsonSpec @Tag
  textLikeJSONValid @Tag
  -- If you remove this, the tests for json for sorters, filters and projections will fail in smos-report-gen
  describe "validTagChar" $ do
    it "says that ')' is invalid" $
      validTagChar ')' `shouldBe` False
    it
      "says that '(' is invalid"
      $ validTagChar '(' `shouldBe` False
  let genLogbookEntryJSON =
        object
          <$> sequence
            [ ("start" .=) <$> (toJSON <$> (genValid :: Gen UTCTime)),
              ("end" .=) <$> (toJSON <$> (genValid :: Gen UTCTime))
            ]
  genValidSpec @Logbook
  jsonSpec @Logbook
  describe "emptyLogbook" $ it "is valid" $ shouldBeValid emptyLogbook
  genJSONValid @Logbook $
    let withGen lbGen =
          sized $ \n -> do
            l <- choose (1, n)
            rest <- replicateM l lbGen
            first <-
              object
                <$> sequence
                  [ ("start" .=) <$> (toJSON <$> (genValid :: Gen UTCTime)),
                    ("end" .=) <$> (toJSON <$> (genValid :: Gen (Maybe UTCTime)))
                  ]
            pure $ toJSON $ first : rest
        genOrderedLogbookEntryJSON = do
          start <- genValid :: Gen UTCTime
          end <- genValid `suchThat` (>= start) :: Gen UTCTime
          pure $ object ["start" .= start, "end" .= end]
     in oneof [withGen genLogbookEntryJSON, withGen genOrderedLogbookEntryJSON]
  genValidSpec @LogbookEntry
  jsonSpec @LogbookEntry
  genJSONValid @LogbookEntry genLogbookEntryJSON
  eqSpec @Entry
  ordSpec @Entry
  genValidSpec @Entry
  jsonSpec @Entry
  genValidSpec @SmosFile
  eqSpec @SmosFile
  ordSpec @SmosFile
  jsonSpec @SmosFile
  genValidSpec @LocalSecond
  eqSpec @LocalSecond
  ordSpec @LocalSecond
  jsonSpec @LocalSecond
  describe "localSecondToLocalTime" $
    it "roundtrips with localTimeToLocalSecond" $
      forAllValid $ \ls ->
        localTimeToLocalSecond (localSecondToLocalTime ls) `shouldBe` ls
  describe "utcSecondToUTCTime" $
    it "roundtrips with utcTimeToUTCSecond" $
      forAllValid $ \ls ->
        utcTimeToUTCSecond (utcSecondToUTCTime ls) `shouldBe` ls
  xdescribe "these fail for reasons related to timezone generation" $ do
    describe "utcSecondToLocalSecond" $
      it "roundtrips with localSecondToUTCSecond" $
        forAllValid $ \tz ->
          forAllValid $ \utcs ->
            localSecondToUTCSecond tz (utcSecondToLocalSecond tz utcs) `shouldBe` utcs
    describe "localSecondToUTCSecond" $
      it "roundtrips with utcSecondToLocalSecond" $
        forAllValid $ \tz ->
          forAllValid $ \utcs ->
            utcSecondToLocalSecond tz (localSecondToUTCSecond tz utcs) `shouldBe` utcs
  describe "parseLocalSecondString" $ do
    it "parses whatever renderLocalSecondString" $
      forAllValid $ \ls ->
        parseLocalSecondString (renderLocalSecondString ls) `shouldBe` Right ls
    describe "can parse these examples from data formats that have been used" $ do
      let expected = LocalSecond {localSecondDay = fromGregorian 2022 01 22, localSecondOfDay = SecondOfDay 1234}
      let p s = it ("can parse " <> show s) $ parseLocalSecondString s `shouldBe` Right expected
      p "2022-01-22 00:20:34.000000000000"
      p "2022-01-22 00:20:34.00000000000"
      p "2022-01-22 00:20:34.0000000000"
      p "2022-01-22 00:20:34.000000000"
      p "2022-01-22 00:20:34.00000000"
      p "2022-01-22 00:20:34.0000000"
      p "2022-01-22 00:20:34.000000"
      p "2022-01-22 00:20:34.00000"
      p "2022-01-22 00:20:34.0000"
      p "2022-01-22 00:20:34.000"
      p "2022-01-22 00:20:34.00"
      p "2022-01-22 00:20:34.0"
      p "2022-01-22 00:20:34"
  describe "parseLocalSecondText" $
    it "parses whatever renderLocalSecondText" $
      forAllValid $ \ls ->
        parseLocalSecondText (renderLocalSecondText ls) `shouldBe` Right ls
  genValidSpec @SecondOfDay
  eqSpec @SecondOfDay
  ordSpec @SecondOfDay
  describe "secondOfDayToTimeOfDay" $
    it "roundtrips with timeOfDayToSecondOfDay" $
      forAllValid $ \sod ->
        timeOfDayToSecondOfDay (secondOfDayToTimeOfDay sod) `shouldBe` sod
  describe "secondOfDayToDiffTime" $
    it "roundtrips with diffTimeToSecondOfDay" $
      forAllValid $ \sod ->
        diffTimeToSecondOfDay (secondOfDayToDiffTime sod) `shouldBe` sod

textLikeJSONValid ::
  forall a.
  (Validity a, Show a, Typeable a, FromJSON a) =>
  Spec
textLikeJSONValid = genJSONValid @a $ JSON.String <$> genValid

genJSONValid ::
  forall a.
  (Validity a, Show a, Typeable a, FromJSON a) =>
  Gen JSON.Value ->
  Spec
genJSONValid gen =
  describe (unwords ["JSON", nameOf @a]) $
    it "parses every value into a valid value" $
      forAll gen $
        \j ->
          case fromJSON j of
            JSON.Error _ -> pure ()
            JSON.Success h -> shouldBeValid (h :: a)
