{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.PostChangePassphraseSpec
  ( spec
  ) where

import TestImport

import Intray.Client

import Intray.API.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
  withIntrayServer $
  describe "PostChangePassphrase" $ do
    it "fails without PermitAdminGetAccounts" $ \cenv ->
      forAllValid $ \cp ->
        failsWithOutPermission cenv PermitPostChangePassphrase $ \t ->
          clientPostChangePassphrase t cp
    it
      "changes a users' password so that you can't log in with the old password anymore in this example" $ \cenv ->
      withValidNewUserAndData cenv $ \un oldPw token ->
        let newPw = "test"
         in do NoContent <-
                 runClientOrError cenv $
                 clientPostChangePassphrase token $
                 ChangePassphrase {changePassphraseOld = oldPw, changePassphraseNew = newPw}
               errOrRes <-
                 runClient cenv $
                 clientPostLogin LoginForm {loginFormUsername = un, loginFormPassword = oldPw}
               case errOrRes of
                 Right _ -> expectationFailure "should not have been able to login."
                 Left _ -> pure ()
    it "changes a users' password so that you can log in with the new password in this example" $ \cenv ->
      withValidNewUserAndData cenv $ \un oldPw token ->
        let newPw = "test"
         in do NoContent <-
                 runClientOrError cenv $
                 clientPostChangePassphrase token $
                 ChangePassphrase {changePassphraseOld = oldPw, changePassphraseNew = newPw}
               token' <- login cenv un newPw
               accountInfo <- runClientOrError cenv $ clientGetAccountInfo token'
               shouldBeValid accountInfo
