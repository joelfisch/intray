{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.GetAccountInfo
    ( serveGetAccountInfo
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

serveGetAccountInfo :: AuthResult AuthCookie -> IntrayHandler AccountInfo
serveGetAccountInfo (Authenticated AuthCookie {..}) =
    withPermission authCookiePermissions PermitGetAccountInfo $ do
        admins <- asks envAdmins
        mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
        case mUser of
            Nothing -> throwError err404 {errBody = "User not found."}
            Just (Entity _ User {..}) -> do
                c <-
                    runDb $
                    count
                        ([IntrayItemUserId ==. authCookieUserUUID] :: [Filter IntrayItem])
                pure
                    AccountInfo
                    { accountInfoUUID = authCookieUserUUID
                    , accountInfoUsername = userUsername
                    , accountInfoCreatedTimestamp = userCreatedTimestamp
                    , accountInfoLastLogin = userLastLogin
                    , accountInfoAdmin = userUsername `elem` admins
                    , accountInfoCount = c
                    }
serveGetAccountInfo _ = throwAll err401
