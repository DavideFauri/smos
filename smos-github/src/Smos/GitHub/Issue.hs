{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.GitHub.Issue where

import Control.Monad
import qualified Data.Text as T
import GHC.Generics (Generic)
import GitHub (IssueNumber (..), Owner, Repo)
import GitHub.Data.Name
import Network.URI
import Text.Read

parseGitHubUrl :: String -> Maybe GitHubUrl
parseGitHubUrl url = do
  uri <- parseURI url
  ua <- uriAuthority uri
  guard (uriRegName ua == "github.com")
  let segments = pathSegments uri
  -- Url is of this form
  -- https://github.com/owner/repo/pull/167
  -- https://github.com/owner/repo/issues/167
  let mkN = N . T.pack
  case segments of
    (owner : repo : "pull" : num : _) -> PullRequestUrl (mkN owner) (mkN repo) <$> (IssueNumber <$> readMaybe num)
    (owner : repo : "issues" : num : _) -> IssueUrl (mkN owner) (mkN repo) <$> (IssueNumber <$> readMaybe num)
    _ -> Nothing

data GitHubUrl
  = PullRequestUrl (Name Owner) (Name Repo) IssueNumber
  | IssueUrl (Name Owner) (Name Repo) IssueNumber
  deriving (Show, Eq, Generic)
