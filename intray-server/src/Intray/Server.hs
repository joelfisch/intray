{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server
  ( runIntrayServer
  , makeIntrayServer
  , intrayAppContext
  ) where

import Import

import Data.Cache

import Control.Concurrent.Async
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic

import Intray.API

import Intray.Server.OptParse.Types
import Intray.Server.Types

import Intray.Server.SigningKey

import Intray.Server.Looper (LoopersSettings(..), runIntrayServerLoopers)
import Intray.Server.Serve (intrayServer)

runIntrayServer :: ServeSettings -> IO ()
runIntrayServer ServeSettings {..} =
  runStderrLoggingT $
  filterLogger (\_ ll -> ll >= serveSetLogLevel) $
  withSqlitePoolInfo serveSetConnectionInfo 1 $ \pool -> do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    signingKey <- liftIO loadSigningKey
    let jwtCfg = defaultJWTSettings signingKey
    let cookieCfg = defaultCookieSettings
    planCache <- liftIO $ newCache Nothing
    let intrayEnv =
          IntrayServerEnv
            { envHost = serveSetHost
            , envConnectionPool = pool
            , envCookieSettings = cookieCfg
            , envJWTSettings = jwtCfg
            , envAdmins = serveSetAdmins
            , envMonetisation =
                (\MonetisationSettings {..} ->
                   MonetisationEnv
                     { monetisationEnvStripeSettings = monetisationSetStripeSettings
                     , monetisationEnvMaxItemsFree = monetisationSetMaxItemsFree
                     }) <$>
                serveSetMonetisationSettings
            , envPlanCache = planCache
            }
    let mLoopersSets =
          case serveSetMonetisationSettings of
            Nothing -> Nothing
            Just MonetisationSettings {..} ->
              Just
                LoopersSettings
                  { loopersSetLogLevel = serveSetLogLevel
                  , loopersSetConnectionPool = pool
                  , loopersSetStripeSettings = monetisationSetStripeSettings
                  , loopersSetStripeEventsFetcher = monetisationSetStripeEventsFetcher
                  , loopersSetStripeEventsRetrier = monetisationSetStripeEventsRetrier
                  }
    let runServer = Warp.run serveSetPort $ intrayApp intrayEnv
    case mLoopersSets of
      Nothing -> liftIO runServer
      Just ls -> do
        let runLoopers = runIntrayServerLoopers ls
        liftIO $ race_ runServer runLoopers

intrayApp :: IntrayServerEnv -> Wai.Application
intrayApp se = addPolicy . serveWithContext intrayAPI (intrayAppContext se) $ makeIntrayServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
      simpleCorsResourcePolicy
        {corsRequestHeaders = ["content-type"], corsMethods = ["GET", "POST", "HEAD", "DELETE"]}

makeIntrayServer :: IntrayServerEnv -> Server IntrayAPI
makeIntrayServer cfg =
  hoistServerWithContext
    intrayAPI
    (Proxy :: Proxy IntrayContext)
    (`runReaderT` cfg)
    (toServant intrayServer)

intrayAppContext :: IntrayServerEnv -> Context IntrayContext
intrayAppContext IntrayServerEnv {..} = envCookieSettings :. envJWTSettings :. EmptyContext

type IntrayContext = '[ CookieSettings, JWTSettings]
