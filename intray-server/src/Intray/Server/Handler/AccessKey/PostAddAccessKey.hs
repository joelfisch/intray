{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.AccessKey.PostAddAccessKey
    ( servePostAddAccessKey
    ) where

import Import

import qualified Data.Set as S
import Data.Time

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostAddAccessKey ::
       AuthResult AuthCookie -> AddAccessKey -> IntrayHandler AccessKeyCreated
servePostAddAccessKey (Authenticated AuthCookie {..}) AddAccessKey {..} =
    withPermission authCookiePermissions PermitPostAddAccessKey $ do
        let perms =
                authCookiePermissions `S.intersection` addAccessKeyPermissions
        unless (perms == addAccessKeyPermissions) $ throwAll err401
        uuid <- liftIO nextRandomUUID
        now <- liftIO getCurrentTime
        secret <- liftIO generateRandomAccessKeySecret
        mhp <- liftIO $ passwordHash $ accessKeySecretText secret
        case mhp of
            Nothing -> throwAll err500 {errBody = "Unable to hash secret key."}
            Just hp -> do
                runDb $
                    insert_
                        AccessKey
                        { accessKeyIdentifier = uuid
                        , accessKeyUser = authCookieUserUUID
                        , accessKeyName = addAccessKeyName
                        , accessKeyHashedKey = hp
                        , accessKeyCreatedTimestamp = now
                        , accessKeyPermissions = perms
                        }
                pure
                    AccessKeyCreated
                    { accessKeyCreatedCreatedTimestamp = now
                    , accessKeyCreatedKey = secret
                    , accessKeyCreatedUUID = uuid
                    }
servePostAddAccessKey _ _ = throwAll err401
