{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.SyncSpec
  ( spec
  ) where

import TestImport

import Intray.Client

import Intray.API.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
  withIntrayServer $
  describe "PostSync" $ do
    it "fails without PermitSync" $ \cenv ->
      forAllValid $ \syncRequest ->
        failsWithOutPermission cenv PermitSync $ \t -> clientPostSync t syncRequest
    it "produces a valid sync result for any sync request" $ \cenv ->
      forAllValid $ \syncRequest ->
        withValidNewUser cenv $ \token -> do
          sr <- runClientOrError cenv $ clientPostSync token syncRequest
          shouldBeValid sr
    it "is idempotent" $ \cenv ->
      forAllValid $ \initStore ->
        withValidNewUser cenv $ \token -> do
          sr1 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest initStore
          let firstStore = mergeSyncResponse initStore sr1
          sr2 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest firstStore
          let secondStore = mergeSyncResponse firstStore sr2
          secondStore `shouldBe` firstStore
