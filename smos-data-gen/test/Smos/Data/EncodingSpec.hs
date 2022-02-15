{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Data.EncodingSpec
  ( spec,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Function
import Data.Functor.Classes
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Smos.Data
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  roundtripSpec "smosFileYamlBS" smosFileYamlBS
  roundtripSpec "smosFileJSONBS" (LB.toStrict . smosFileJSONBS)
  roundtripSpec "smosFileJSONPrettyBS" (LB.toStrict . smosFileJSONPrettyBS)

roundtripSpec :: String -> (SmosFile -> ByteString) -> Spec
roundtripSpec name func =
  describe name $ do
    it "produces bytestrings that can be roundtripped 'close enough' with parseSmosFile" $
      forAllValid $ \sf ->
        let bs = func sf
            prettyBs = T.unpack $ TE.decodeUtf8 bs
         in case parseSmosFile bs of
              Left pe ->
                expectationFailure $
                  unlines
                    [ "Parsing should not have failed",
                      "encoding the following value:",
                      ppShow sf,
                      "produced the folling bytestring:",
                      prettyBs,
                      "but parsing failed with the following error:",
                      show pe
                    ]
              Right sf' ->
                let ctx =
                      unlines
                        [ name ++ " should have roundtripped with parseSmosFile",
                          "started with:",
                          ppShow sf,
                          "encoding produced the following value:",
                          prettyBs,
                          "expected:",
                          ppShow sf',
                          "actual:",
                          ppShow sf
                        ]
                 in context ctx $ when (not (closeEnough sf' sf)) $ sf' `shouldBe` sf

    it "produces bytestrings that can be roundtripped with parseSmosFile when you render again afterwards" $
      forAllValid $ \sf ->
        let bs = func sf
            prettyBs = T.unpack $ TE.decodeUtf8 bs
         in case parseSmosFile bs of
              Left pe ->
                expectationFailure $
                  unlines
                    [ "Parsing should not have failed",
                      "encoding the following value:",
                      ppShow sf,
                      "produced the folling bytestring:",
                      prettyBs,
                      "but parsing failed with the following error:",
                      show pe
                    ]
              Right sf' -> func sf' `shouldBe` bs

    it "produces bytestrings that can be roundtripped with parseSmosFile when you render twice" $
      forAllValid $ \sf ->
        let bs = func sf
            prettyBs = T.unpack $ TE.decodeUtf8 bs
         in case parseSmosFile bs of
              Left pe ->
                expectationFailure $
                  unlines
                    [ "Parsing should not have failed",
                      "encoding the following value:",
                      ppShow sf,
                      "produced the folling bytestring:",
                      prettyBs,
                      "but parsing failed with the following error:",
                      show pe
                    ]
              Right sf' -> parseSmosFile (func sf') `shouldBe` parseSmosFile bs

closeEnough :: SmosFile -> SmosFile -> Bool
closeEnough sf1 sf2 = liftEq (liftEq closeEnoughEntry) (smosFileForest sf1) (smosFileForest sf2)
  where
    closeEnoughEntry :: Entry -> Entry -> Bool
    closeEnoughEntry e1 e2 =
      let Entry _ _ _ _ _ _ _ = undefined
       in and
            [ ((==) `on` entryHeader) e1 e2,
              ((==) `on` entryContents) e1 e2,
              (liftEq closeEnoughTimestamp `on` entryTimestamps) e1 e2,
              ((==) `on` entryProperties) e1 e2,
              (closeEnoughStateHistory `on` entryStateHistory) e1 e2,
              ((==) `on` entryTags) e1 e2,
              (closeEnoughLogbook `on` entryLogbook) e1 e2
            ]

    closeEnoughTimestamp :: Timestamp -> Timestamp -> Bool
    closeEnoughTimestamp = closeEnoughLocalTime `on` timestampLocalTime

    closeEnoughStateHistory :: StateHistory -> StateHistory -> Bool
    closeEnoughStateHistory = liftEq closeEnoughStateHistoryEntry `on` unStateHistory

    closeEnoughStateHistoryEntry :: StateHistoryEntry -> StateHistoryEntry -> Bool
    closeEnoughStateHistoryEntry =
      let StateHistoryEntry _ _ = undefined
       in closeEnoughUTCTime `on` stateHistoryEntryTimestamp

    closeEnoughLogbook :: Logbook -> Logbook -> Bool
    closeEnoughLogbook lb1 lb2 =
      case (lb1, lb2) of
        (LogOpen start1 lbes1, LogOpen start2 lbes2) ->
          and
            [ closeEnoughUTCTime start1 start2,
              liftEq closeEnoughLogbookEntry lbes1 lbes2
            ]
        (LogClosed lbes1, LogClosed lbes2) ->
          liftEq closeEnoughLogbookEntry lbes1 lbes2
        _ -> False

    closeEnoughLogbookEntry :: LogbookEntry -> LogbookEntry -> Bool
    closeEnoughLogbookEntry lbe1 lbe2 =
      let LogbookEntry _ _ = undefined
       in and
            [ (closeEnoughUTCTime `on` logbookEntryStart) lbe1 lbe2,
              (closeEnoughUTCTime `on` logbookEntryEnd) lbe1 lbe2
            ]

    closeEnoughLocalTime :: LocalTime -> LocalTime -> Bool
    closeEnoughLocalTime = closeEnoughUTCTime `on` localTimeToUTC utc

    closeEnoughUTCTime :: UTCTime -> UTCTime -> Bool
    closeEnoughUTCTime u1 u2 = diffUTCTime u1 u2 < 1 -- Less than a second doesn't matter.
