module Intray.Web.Server.Handler.HomeRSpec where

import TestImport

import Yesod.Test

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils

spec :: Spec
spec =
  intrayWebServerSpec $
  ydescribe "HomeR" $ do
    yit "gets a 200 for non-logged-in user" $ do
      get HomeR
      statusIs 200
    yit "gets a 200 for an example user" $
      withExampleAccount_ $ do
        get HomeR
        statusIs 200
