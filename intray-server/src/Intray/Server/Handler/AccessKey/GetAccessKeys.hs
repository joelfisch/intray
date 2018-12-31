{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.GetAccessKeys
    ( serveGetAccessKeys
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.AccessKey.GetAccessKey
       (makeAccessKeyInfo)
import Intray.Server.Handler.Utils

serveGetAccessKeys :: AuthResult AuthCookie -> IntrayHandler [AccessKeyInfo]
serveGetAccessKeys (Authenticated AuthCookie {..}) =
    withPermission authCookiePermissions PermitGetAccessKeys $ do
        aks <- runDb $ selectList [AccessKeyUser ==. authCookieUserUUID] []
        pure $ map (makeAccessKeyInfo . entityVal) aks
serveGetAccessKeys _ = throwAll err401
