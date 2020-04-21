{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Server.Handler.Admin.GetAccounts
  ( serveAdminGetAccounts
  ) where

import Database.Persist
import Import
import Intray.API
import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Types

serveAdminGetAccounts :: AuthCookie -> IntrayHandler [AccountInfo]
serveAdminGetAccounts AuthCookie {..} = do
  admins <- asks envAdmins
  users <- runDb $ selectList [] [Desc UserLastLogin]
  forM users $ \(Entity _ User {..}) -> do
    c <- runDb $ count ([IntrayItemUserId ==. userIdentifier] :: [Filter IntrayItem])
    ups <- getUserPaidStatus userIdentifier
    pure
      AccountInfo
        { accountInfoUUID = userIdentifier
        , accountInfoUsername = userUsername
        , accountInfoCreatedTimestamp = userCreatedTimestamp
        , accountInfoLastLogin = userLastLogin
        , accountInfoAdmin = userUsername `elem` admins
        , accountInfoCount = c
        , accountInfoStatus = ups
        }
