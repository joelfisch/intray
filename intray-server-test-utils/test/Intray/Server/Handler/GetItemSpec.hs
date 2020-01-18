{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.GetItemSpec
  ( spec
  ) where

import TestImport

import Intray.API
import Intray.Client

import Intray.API.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
  withIntrayServer $
  describe "GetItem" $
  it "gets the same item that was just added" $ \cenv ->
    forAllValid $ \t ->
      withValidNewUser cenv $ \token -> do
        i <-
          runClientOrError cenv $ do
            uuid <- clientPostAddItem token t
            clientGetItem token uuid
        itemInfoContents i `shouldBe` t
