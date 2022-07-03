{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import Smos.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = do
  mDocsUrl <- getsYesod appDocsBaseUrl
  withNavBar $ do
    $(widgetFile "home")
