{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.Admin.GetAccounts
    ( serveAdminGetAccounts
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

serveAdminGetAccounts :: AuthResult AuthCookie -> IntrayHandler [AccountInfo]
serveAdminGetAccounts (Authenticated AuthCookie {..}) =
    withPermission authCookiePermissions PermitAdminGetAccounts $ do
        admins <- asks envAdmins
        users <- runDb $ selectList [] [Asc UserId]
        forM users $ \(Entity _ User {..}) -> do
            c <-
                runDb $
                count
                    ([IntrayItemUserId ==. userIdentifier] :: [Filter IntrayItem])
            pure
                AccountInfo
                { accountInfoUUID = userIdentifier
                , accountInfoUsername = userUsername
                , accountInfoCreatedTimestamp = userCreatedTimestamp
                , accountInfoLastLogin = userLastLogin
                , accountInfoAdmin = userUsername `elem` admins
                , accountInfoCount = c
                }
serveAdminGetAccounts _ = throwAll err401
