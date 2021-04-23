{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetNextActionReport
  ( serveGetNextActionReport,
  )
where

import Smos.Report.Next
import Smos.Server.Handler.Import

serveGetNextActionReport :: AuthCookie -> ServerHandler NextActionReport
serveGetNextActionReport AuthCookie {..} = withUserId authCookieUsername $ \uid ->
  streamSmosFiles uid HideArchive (nextActionReportConduit Nothing)
