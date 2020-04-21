{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server
  ( intrayWebServer
  , makeIntrayApp
  ) where

import Control.Concurrent
import Control.Concurrent.Async (concurrently_)
import qualified Data.HashMap.Strict as HM
import Import
import qualified Intray.Server as API
import qualified Intray.Server.OptParse as API
import Intray.Web.Server.Application ()
import Intray.Web.Server.Foundation
import Intray.Web.Server.OptParse
import qualified Network.HTTP.Client as Http
import Servant.Client (parseBaseUrl)
import Yesod

intrayWebServer :: IO ()
intrayWebServer = do
  (DispatchServe ss, Settings) <- getInstructions
  pPrint ss
  concurrently_ (runIntrayWebServer ss) (runIntrayAPIServer ss)

runIntrayWebServer :: ServeSettings -> IO ()
runIntrayWebServer ss@ServeSettings {..} = do
  app <- makeIntrayApp ss
  warp serveSetPort app

makeIntrayApp :: ServeSettings -> IO App
makeIntrayApp ServeSettings {..} = do
  man <- Http.newManager Http.defaultManagerSettings
  tokens <- newMVar HM.empty
  burl <- parseBaseUrl $ "http://127.0.0.1:" ++ show (API.serveSetPort serveSetAPISettings)
  pure
    App
      { appHttpManager = man
      , appStatic = myStatic
      , appTracking = serveSetTracking
      , appVerification = serveSetVerification
      , appPersistLogins = serveSetPersistLogins
      , appLoginTokens = tokens
      , appAPIBaseUrl = burl
      }

runIntrayAPIServer :: ServeSettings -> IO ()
runIntrayAPIServer ss = do
  let apiServeSets = serveSetAPISettings ss
  API.runIntrayServer apiServeSets
