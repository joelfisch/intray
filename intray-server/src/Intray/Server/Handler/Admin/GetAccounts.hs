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

import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.GetAccountInfo
import Intray.Server.Handler.Utils

serveAdminGetAccounts :: AuthCookie -> IntrayHandler [AccountInfo]
serveAdminGetAccounts AuthCookie {..} = do
  admins <- asks envAdmins
  users <- runDb $ selectList [] [Asc UserId]
  forM users $ \(Entity _ User {..}) -> do
    c <- runDb $ count ([IntrayItemUserId ==. userIdentifier] :: [Filter IntrayItem])
    subbed <- getAccountSubscribed userIdentifier
    pure
      AccountInfo
        { accountInfoUUID = userIdentifier
        , accountInfoUsername = userUsername
        , accountInfoCreatedTimestamp = userCreatedTimestamp
        , accountInfoLastLogin = userLastLogin
        , accountInfoAdmin = userUsername `elem` admins
        , accountInfoCount = c
        , accountInfoSubscribed = subbed
        }
