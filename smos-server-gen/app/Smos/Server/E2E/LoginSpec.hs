{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.E2E.LoginSpec (spec) where

import Control.Monad.Reader
import Data.SemVer
import Servant.Client
import Smos.Client
import Smos.Server.E2E.TestUtils
import Test.Syd

spec :: Version -> TestDef '[ClientEnv] ()
spec serverVersion =
  -- User deletion (cleanup) was introduced in version 0.1.0
  when (serverVersion >= version 0 1 0 [] []) $ do
    beforeAllWith cleanupTestUser $ do
      describe "PostRegister" $
        itWithOuter "can register with a test user" $ \cenv -> do
          NoContent <-
            runClientOrDie cenv $
              clientPostRegister
                Register
                  { registerUsername = testUsername,
                    registerPassword = testPassword
                  }
          pure ()
      describe "PostLogin" $
        itWithOuter "can login with that test user" $ \cenv -> do
          errOrToken <-
            runClientOrDie cenv $
              clientLogin
                Login
                  { loginUsername = testUsername,
                    loginPassword = testPassword
                  }
          case errOrToken of
            Left err -> expectationFailure $ show err
            Right _ -> pure ()
      describe "GetUserPermissions" $
        itWithOuter "can get the test user's permissions" $ \cenv -> do
          withTestLogin cenv $ \t -> do
            UserPermissions {..} <- runClientOrDie cenv $ clientGetUserPermissions t
            userPermissionsIsAdmin `shouldBe` False
      describe "DeleteUser" $ do
        itWithOuter "can delete the test user" $ \cenv -> do
          withTestLogin cenv $ \t -> do
            NoContent <- runClientOrDie cenv $ clientDeleteUser t
            pure ()
