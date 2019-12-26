{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AdminRSpec where

import TestImport

import Yesod.Test

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
  intrayWebServerSpec $
  ydescribe "AdminR" $ do
    yit "gets a 200 when logged in as admin" $
      withAdminAccount_ $ do
        get AdminR
        statusIs 200
    yit "gets a 404 when not logged in as admin" $
      withExampleAccount_ $ do
        get AdminR
        statusIs 404
