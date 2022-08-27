{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.Admin.GetUsers
  ( serveGetUsers,
  )
where

import Smos.Server.Handler.Import

serveGetUsers :: AuthNCookie -> ServerHandler [UserInfo]
serveGetUsers ac = asAdmin (authNCookieUsername ac) $ do
  userEntities <- runDB $ selectList [] [Desc UserLastUse, Desc UserLastLogin, Desc UserCreated]
  mServerAdmin <- asks serverEnvAdmin
  forM userEntities $ \(Entity _ User {..}) -> do
    pure $
      UserInfo
        { userInfoUsername = userName,
          userInfoAdmin = mServerAdmin == Just userName,
          userInfoCreated = userCreated,
          userInfoLastLogin = userLastLogin,
          userInfoLastUse = userLastUse
        }
