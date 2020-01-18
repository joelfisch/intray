{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.Admin.GetStats
  ( serveAdminGetStats
  ) where

import Import

import Database.Persist

import Data.Time

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveAdminGetStats :: AuthResult AuthCookie -> IntrayHandler AdminStats
serveAdminGetStats (Authenticated AuthCookie {..}) =
  withPermission authCookiePermissions PermitAdminGetStats $ do
    adminStatsNbAccounts <- fmap fromIntegral $ runDb $ count ([] :: [Filter User])
    adminStatsNbItems <- fmap fromIntegral $ runDb $ count ([] :: [Filter IntrayItem])
    now <- liftIO getCurrentTime
    let day :: NominalDiffTime
        day = 86400
    let activeUsers time =
          fmap fromIntegral $ runDb $ count [UserLastLogin >=. Just (addUTCTime (-time) now)]
    activeUsersDaily <- activeUsers day
    activeUsersWeekly <- activeUsers $ 7 * day
    activeUsersMonthly <- activeUsers $ 30 * day
    activeUsersYearly <- activeUsers $ 365 * day
    let adminStatsActiveUsers = ActiveUsers {..}
    pure AdminStats {..}
serveAdminGetStats _ = throwAll err401
