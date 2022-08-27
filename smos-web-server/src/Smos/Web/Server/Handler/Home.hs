{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Home where

import qualified Data.Text as T
import Smos.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = do
  mDocsUrl <- getsYesod appDocsBaseUrl
  withNavBar $ do
    addScript $ StaticR asciinema_player_js
    addStylesheet $ StaticR asciinema_player_css

    $(widgetFile "home")
