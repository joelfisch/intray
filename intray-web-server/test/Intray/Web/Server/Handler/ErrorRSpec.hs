{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.ErrorRSpec where

import TestImport

import Yesod.Test

import Intray.Web.Server (makeIntrayApp)

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils

spec :: Spec
spec = do
  intrayWebServerSpec $
    ydescribe "ErrorAPIDownR" $
    yit "gets a 200 for non-logged-in user" $ do
      get $ ErrorAPIDownR "example"
      statusIs 200
  let makeServer = intrayTestServeSettings >>= makeIntrayApp
  yesodSpecWithSiteGenerator makeServer $ do
    ydescribe "ErrorAPIDownR" $
      yit "gets a 200 when the API is down" $ do
        get $ ErrorAPIDownR "example"
        statusIs 200
        bodyContains "The Intray API is down."
        bodyContains "example"
    ydescribe "APIDocsR" $
      yit "redirects to ErrorAPIDownR" $ do
        get APIDocsR
        statusIs 303
        loc <- getLocation
        case loc of
          Right (ErrorAPIDownR _) -> do
            void followRedirect
            statusIs 200
          _ -> liftIO $ expectationFailure $ unwords ["Should have redirected:", show loc]
