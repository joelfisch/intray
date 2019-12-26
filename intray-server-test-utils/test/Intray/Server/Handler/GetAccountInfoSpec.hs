{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.GetAccountInfoSpec
  ( spec
  ) where

import TestImport

import Intray.Client

import Intray.Data.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
  withIntrayServer $
  describe "GetAccountInfo" $ do
    it "fails without PermitGetAccountInfo" $ \cenv ->
      failsWithOutPermission cenv PermitGetAccountInfo clientGetAccountInfo
    it "returns valid account info" $ \cenv ->
      withValidNewUser cenv $ \token -> do
        accountInfo <- runClientOrError cenv $ clientGetAccountInfo token
        shouldBeValid accountInfo
    it "gets account info with the right username" $ \cenv ->
      withValidNewUserAndData cenv $ \un _ token -> do
        accountInfo <- runClientOrError cenv $ clientGetAccountInfo token
        accountInfoUsername accountInfo `shouldBe` un
