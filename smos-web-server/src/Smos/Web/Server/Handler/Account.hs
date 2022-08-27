{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Account
  ( getAccountR,
    postAccountDeleteR,
  )
where

import Control.Monad.Except
import Smos.Web.Server.Handler.Import

getAccountR :: Handler Html
getAccountR = withLogin' $ \un t -> do
  now <- liftIO getCurrentTime
  withNavBar $ do
    token <- genToken
    $(widgetFile "account")

postAccountDeleteR :: Handler ()
postAccountDeleteR = withLogin' $ \un t -> do
  NoContent <- runClientOrErr $ clientDeleteUser t
  addMessage "is-success" "Account succesfully deleted."
  deleteLoginToken un
  clearCreds True
