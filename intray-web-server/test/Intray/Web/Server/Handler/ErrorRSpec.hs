{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.ErrorRSpec where

import Intray.Data
import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils
import qualified Network.HTTP.Client as Http
import Servant.Client
import TestImport
import Yesod.Test

spec :: Spec
spec = do
  intrayWebServerSpec $
    ydescribe "ErrorAPIDownR" $
    yit "gets a 200 for non-logged-in user" $ do
      get $ ErrorAPIDownR "example"
      statusIs 200
  before
    (do man <- liftIO $ Http.newManager Http.defaultManagerSettings
        burl <- parseBaseUrl "localhost:8000" -- but this one doesn't exist
        pure $ mkClientEnv man burl) $
    withConnectionPoolToo $
    withWebServer $ do
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
