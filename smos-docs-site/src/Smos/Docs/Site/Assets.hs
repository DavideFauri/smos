{-# LANGUAGE TemplateHaskell #-}

module Smos.Docs.Site.Assets where

import Smos.Docs.Site.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkEmbeddedStatic
  development
  "assets"
  ( let remoteStatic fp = embedRemoteFileAt fp ("static/" ++ fp)
     in [ embedDir "content/assets",
          remoteStatic "font-awesome.css" "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css",
          remoteStatic "favicon.ico" "https://cs-syd.eu/logo/res/favicon.ico"
        ]
  )
