{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.Command.ImportSpec (spec) where

import Smos.GitHub.Command.Import
import Smos.GitHub.Issue.Gen ()
import Test.Syd
import Test.Syd.Validity

instance GenValid ImportDetails

spec :: Spec
spec = do
  describe "renderProjectPath" $
    it "produces valid paths" $
      producesValid renderProjectPath
  describe "renderSmosProjects" $
    it "produces valid smosFiles" $
      producesValid3 renderSmosProject
