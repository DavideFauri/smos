{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.Command.ImportSpec (spec) where

import GitHub
import GitHub.Data.Name
import Smos.Data.TestUtils
import Smos.GitHub.Command.Import
import Smos.GitHub.Issue
import Smos.GitHub.Issue.Gen ()
import Test.Syd
import Test.Syd.Validity

instance GenValid ImportDetails

spec :: Spec
spec = do
  describe "renderProjectPath" $
    it "produces valid paths" $
      producesValid renderProjectPath
  describe "renderSmosProjects" $ do
    it "produces valid smosFiles" $
      producesValid3
        renderSmosProject
    it "produces the same smos file for this hypothetical issue" $
      case renderSmosProject
        "https://github.com/NorfairKing/autodocodec/pull/18"
        ( PullRequestUrl
            (N "NorfairKing")
            (N "autodocodec")
            (IssueNumber 18)
        )
        (Just ImportDetails {importDetailTitle = "Disjoint Codecs"}) of
        Nothing -> expectationFailure "should have produced a project."
        Just smosFile ->
          pure $ pureGoldenSmosFile "test_resources/pull-request-with-details.smos" smosFile
    it "produces the same smos file for this hypothetical issue" $
      case renderSmosProject
        "https://github.com/NorfairKing/autodocodec/pull/18"
        ( PullRequestUrl
            (N "NorfairKing")
            (N "autodocodec")
            (IssueNumber 18)
        )
        Nothing of
        Nothing -> expectationFailure "should have produced a project."
        Just smosFile ->
          pure $ pureGoldenSmosFile "test_resources/pull-request-without-details.smos" smosFile
