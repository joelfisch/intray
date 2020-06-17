{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server
  ( intrayWebServer
  ) where

import Control.Concurrent.Async (concurrently_)
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist.Sqlite
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
runIntrayWebServer ServeSettings {..} =
  runStderrLoggingT $
  filterLogger (\_ ll -> ll >= API.serveSetLogLevel serveSetAPISettings) $
  withSqlitePoolInfo (mkSqliteConnectionInfo $ T.pack serveSetLoginCacheFile) 1 $ \pool -> do
    man <- liftIO $ Http.newManager Http.defaultManagerSettings
    burl <- parseBaseUrl $ "http://127.0.0.1:" ++ show (API.serveSetPort serveSetAPISettings)
    let app =
          App
            { appHttpManager = man
            , appStatic = myStatic
            , appTracking = serveSetTracking
            , appVerification = serveSetVerification
            , appAPIBaseUrl = burl
            , appConnectionPool = pool
            }
    liftIO $ do
      runSqlPool (runMigration migrateLoginCache) pool
      warp serveSetPort app

runIntrayAPIServer :: ServeSettings -> IO ()
runIntrayAPIServer ss = do
  let apiServeSets = serveSetAPISettings ss
  API.runIntrayServer apiServeSets
