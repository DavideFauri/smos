{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Docs.Site.Handler.Changelog
  ( getChangelogR,
    getChangelogUnreleasedR,
    getChangelogLatestR,
    getChangelogAllR,
    getChangelogReleaseR,
  )
where

import Data.List
import qualified Data.Map as M
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Language.Haskell.TH.Load
import Smos.Docs.Site.Handler.Import

showReleaseDay :: Day -> Text
showReleaseDay = T.pack . formatTime defaultTimeLocale "%F"

getChangelogR :: Handler Html
getChangelogR = do
  mUnreleased <- loadIO unreleasedChangelog
  (latestReleaseDay, _) <- loadIO latestChangelog
  releases <- loadIO changelogs
  defaultLayout $ do
    setSmosTitle "Changelog"
    setDescriptionIdemp "The changelog for all of the Smos tools and libraries"
    $(widgetFile "changelog")

getChangelogUnreleasedR :: Handler Html
getChangelogUnreleasedR = do
  mUnreleased <- loadIO unreleasedChangelog
  case mUnreleased of
    Nothing -> notFound
    Just unreleased -> defaultLayout $ do
      setSmosTitle "Changelog for unreleased changes"
      setDescriptionIdemp "The changelog for unreleased changes for all of the Smos tools and libraries"
      $(widgetFile "changelog/unreleased")

getChangelogLatestR :: Handler Html
getChangelogLatestR = do
  (latestReleaseDay, _) <- loadIO latestChangelog
  redirect $ ChangelogReleaseR latestReleaseDay

getChangelogAllR :: Handler Html
getChangelogAllR = do
  mUnreleased <- loadIO unreleasedChangelog
  releases <- loadIO changelogs
  defaultLayout $ do
    setSmosTitle "Changelog"
    setDescriptionIdemp "The changelog for all releases of all of the Smos tools and libraries"
    $(widgetFile "changelog/all")

getChangelogReleaseR :: Day -> Handler Html
getChangelogReleaseR day = do
  releases <- loadIO changelogs
  case M.lookup day releases of
    Nothing -> notFound
    Just release -> do
      defaultLayout $ do
        setSmosTitle $ "Changelog for the " <> toHtml (showReleaseDay day) <> " release"
        setDescriptionIdemp $ "The changelog for the " <> showReleaseDay day <> " release of all of the Smos tools and libraries"
        $(widgetFile "changelog/release")
