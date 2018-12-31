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
import Intray.Data

import Intray.Server.OptParse.Types
import Intray.Server.Types

import Intray.Server.SigningKey

import Intray.Server.Handler (intrayServer)

runIntrayServer :: ServeSettings -> IO ()
runIntrayServer ServeSettings {..} =
    runStderrLoggingT $
    withSqlitePoolInfo serveSetConnectionInfo serveSetConnectionCount $ \pool -> do
        runResourceT $ flip runSqlPool pool $ runMigration migrateAll
        signingKey <- liftIO loadSigningKey
        let jwtCfg = defaultJWTSettings signingKey
        let cookieCfg = defaultCookieSettings
        let intrayEnv =
                IntrayServerEnv
                { envConnectionPool = pool
                , envCookieSettings = cookieCfg
                , envJWTSettings = jwtCfg
                , envAdmins = serveSetAdmins
                }
        liftIO $ Warp.run serveSetPort $ intrayApp intrayEnv

intrayApp :: IntrayServerEnv -> Wai.Application
intrayApp se =
    addPolicy . serveWithContext intrayAPI (intrayAppContext se) $
    makeIntrayServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
        simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        , corsMethods = ["GET", "POST", "HEAD", "DELETE"]
        }

makeIntrayServer :: IntrayServerEnv -> Server IntrayAPI
makeIntrayServer cfg =
    hoistServerWithContext
        intrayAPI
        (Proxy :: Proxy IntrayContext)
        (`runReaderT` cfg)
        (toServant intrayServer)

intrayAppContext :: IntrayServerEnv -> Context IntrayContext
intrayAppContext IntrayServerEnv {..} =
    envCookieSettings :. envJWTSettings :. EmptyContext

type IntrayContext = '[ CookieSettings, JWTSettings]
