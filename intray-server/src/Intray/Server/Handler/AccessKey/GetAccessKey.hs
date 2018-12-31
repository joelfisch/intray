{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.GetAccessKey
    ( serveGetAccessKey
    , makeAccessKeyInfo
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveGetAccessKey ::
       AuthResult AuthCookie -> AccessKeyUUID -> IntrayHandler AccessKeyInfo
serveGetAccessKey (Authenticated AuthCookie {..}) uuid =
    withPermission authCookiePermissions PermitGetAccessKey $ do
        mac <- runDb $ getBy $ UniqueAccessKeyIdentifier uuid
        case mac of
            Nothing -> throwAll err404 {errBody = "AccessKey not found."}
            Just (Entity _ ak) -> pure $ makeAccessKeyInfo ak
serveGetAccessKey _ _ = throwAll err401

makeAccessKeyInfo :: AccessKey -> AccessKeyInfo
makeAccessKeyInfo AccessKey {..} =
    AccessKeyInfo
    { accessKeyInfoUUID = accessKeyIdentifier
    , accessKeyInfoName = accessKeyName
    , accessKeyInfoCreatedTimestamp = accessKeyCreatedTimestamp
    , accessKeyInfoPermissions = accessKeyPermissions
    }
