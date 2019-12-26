{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.TestUtils
    ( intrayTestServeSettings
    , intrayWebServerSpec
    , withExampleAccount
    , withExampleAccount_
    , withExampleAccountAndLogin
    , withExampleAccountAndLogin_
    , withAdminAccount
    , withAdminAccount_
    , withAdminAccountAndLogin
    , withAdminAccountAndLogin_
    ) where

import TestImport

import Yesod.Auth
import Yesod.Test

import Data.Text (Text)

import Network.HTTP.Types

import Database.Persist.Sqlite (mkSqliteConnectionInfo)

import Servant.Client (BaseUrl(..), ClientEnv(..))

import Intray.Data

import qualified Intray.Server.TestUtils as API

import Intray.Web.Server
import Intray.Web.Server.Foundation
import Intray.Web.Server.OptParse.Types

import Intray.Data.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

intrayTestServeSettings :: IO ServeSettings
intrayTestServeSettings = do
    let connInfo = mkSqliteConnectionInfo "test.db"
    pure
        ServeSettings
        { serveSetPort = 8000
        , serveSetPersistLogins = False
        , serveSetTracking = Nothing
        , serveSetVerification = Nothing
        , serveSetAPIPort = 8001
        , serveSetAPIConnectionInfo = connInfo
        , serveSetAPIAdmins = [fromJust $ parseUsername "admin"]
        }

intrayWebServerSpec :: YesodSpec App -> Spec
intrayWebServerSpec = b . a
  where
    a :: YesodSpec App -> SpecWith ClientEnv
    a =
        yesodSpecWithSiteGeneratorAndArgument
            (\(ClientEnv _ burl _) -> do
                 sets <- intrayTestServeSettings
                 let sets' = sets {serveSetAPIPort = baseUrlPort burl}
                 makeIntrayApp sets')
    b :: SpecWith ClientEnv -> Spec
    b = API.withIntrayServer

loginTo :: Username -> Text -> YesodExample App ()
loginTo username passphrase = do
    get $ AuthR LoginR
    statusIs 200
    request $ do
        setMethod methodPost
        setUrl $ AuthR loginFormPostTargetR
        addTokenFromCookie
        addPostParam "userkey" $ usernameText username
        addPostParam "passphrase" passphrase
    statusIs 303
    loc <- getLocation
    liftIO $ loc `shouldBe` Right AddR

withFreshAccount ::
       Username
    -> Text
    -> (Username -> Text -> YesodExample App a)
    -> YesodExample App a
withFreshAccount exampleUsername examplePassphrase func = do
    get $ AuthR registerR
    statusIs 200
    request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addTokenFromCookie
        addPostParam "username" $ usernameText exampleUsername
        addPostParam "passphrase" examplePassphrase
        addPostParam "passphrase-confirm" examplePassphrase
    statusIs 303
    loc <- getLocation
    liftIO $ loc `shouldBe` Right AddR
    func exampleUsername examplePassphrase

withExampleAccount ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccount =
    withFreshAccount (fromJust $ parseUsername "example") "pass"

withExampleAccountAndLogin ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccountAndLogin func =
    withExampleAccount $ \un p -> do
        loginTo un p
        func un p

withExampleAccount_ :: YesodExample App a -> YesodExample App a
withExampleAccount_ = withExampleAccount . const . const

withExampleAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withExampleAccountAndLogin_ = withExampleAccountAndLogin . const . const

withAdminAccount ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccount = withFreshAccount (fromJust $ parseUsername "admin") "admin"

withAdminAccount_ :: YesodExample App a -> YesodExample App a
withAdminAccount_ = withAdminAccount . const . const

withAdminAccountAndLogin ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccountAndLogin func =
    withAdminAccount $ \un p -> do
        loginTo un p
        func un p

withAdminAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withAdminAccountAndLogin_ = withAdminAccountAndLogin . const . const
